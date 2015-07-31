//
//  HiSpeed-IIR.c
//  HiSpeed IIR
//
//  Created by David McClain on 7/31/15.
//  Copyright (c) 2015 David McClain. All rights reserved.
//

// #include "HiSpeed-IIR.h"
#include <math.h>
#include <stdlib.h>
#include <Accelerate/Accelerate.h>

static long  tpl_ix = 0;
static float tpl_lbuf[24];
static float tpl_rbuf[24];
/*
;; ------------------------------------------------------------------
;; Let's examine the TruePeak (TP) 4x upsampling filter...
;; FIR Taps for 48 kHz Fsamp
;; Report in units of dB TP when upsample rate >= 192 kHz

;; order 48, 4-phase, FIR interpolating filter
;; DC gain is 12 dB to make up for insertion of zeros
*/

static float tpl_ph0[12] = {
    0.0017089843750f,
    0.0109863281250f,
    -0.0196533203125f,
    0.0332031250000f,
    -0.0594482421875f,
    0.1373291015625f,
    0.9721679687500f,
    -0.1022949218750f,
    0.0476074218750f,
    -0.0266113281250f,
    0.0148925781250f,
    -0.0083007812500f
};
static float tpl_ph1[12] = {
    -0.0291748046875f,
    0.0292968750000f,
    -0.0517578125000f,
    0.0891113281250f,
    -0.1665039062500f,
    0.4650878906250f,
    0.7797851562500f,
    -0.2003173828125f,
    0.1015625000000f,
    -0.0582275390625f,
    0.0330810546875f,
    -0.0189208984375f
};
static float tpl_ph2[12] = {
    -0.0189208984375f,
    0.0330810546875f,
    -0.0582275390625f,
    0.1015625000000f,
    -0.2003173828125f,
    0.7797851562500f,
    0.4650878906250f,
    -0.1665039062500f,
    0.0891113281250f,
    -0.0517578125000f,
    0.0292968750000f,
    -0.0291748046875f
};
static float tpl_ph3[12] = {
    -0.0083007812500f,
    0.0148925781250f,
    -0.0266113281250f,
    0.0476074218750f,
    -0.1022949218750f,
    0.9721679687500f,
    0.1373291015625f,
    -0.0594482421875f,
    0.0332031250000f,
    -0.0196533203125f,
    0.0109863281250f,
    0.0017089843750f
};

static void save_fir_sample(float lsamp, float rsamp)
{
    if(--tpl_ix < 0)
        tpl_ix = 11;
    tpl_lbuf[tpl_ix]    = lsamp;
    tpl_lbuf[tpl_ix+12] = lsamp;
    tpl_rbuf[tpl_ix]    = rsamp;
    tpl_rbuf[tpl_ix+12] = rsamp;
}

static float tpl_accum_phase(float *ph)
{
    float suml, sumr;
   
    vDSP_dotpr(ph, 1, &tpl_lbuf[tpl_ix], 1, &suml, 12);
    vDSP_dotpr(ph, 1, &tpl_rbuf[tpl_ix], 1, &sumr, 12);
    suml = fabsf(suml);
    sumr = fabsf(sumr);
    return (suml > sumr ? suml : sumr);
}

static float tpl_absmax()
{
    float tpl1, tpl2;

    tpl1 = tpl_accum_phase(tpl_ph0);
    tpl2 = tpl_accum_phase(tpl_ph1);
    if(tpl2 > tpl1)
        tpl1 = tpl2;
    tpl2 = tpl_accum_phase(tpl_ph2);
    if(tpl2 > tpl1)
        tpl1 = tpl2;
    tpl2 = tpl_accum_phase(tpl_ph3);
    return (tpl2 > tpl1 ? tpl2 : tpl1);
}

extern "C"
float hsiir_tpl(float* pbuf, long nsamp)
{
    float tpl_max = 0.0f;
    
    while(--nsamp >= 0)
    {
        float tpl;

        save_fir_sample(pbuf[0], pbuf[1]);
        pbuf += 2;
        tpl = tpl_absmax();
        if(tpl > tpl_max)
            tpl_max = tpl;
    }
    return tpl_max;
}

static float hsiir_coffs[10];
static float hsiir_lbuf[6];
static float hsiir_rbuf[6];

extern "C"
void hsiir_init(float *pcoffs)
{
    memcpy(hsiir_coffs, pcoffs, 10*sizeof(float));
    memset(hsiir_lbuf,0,sizeof(hsiir_lbuf));
    memset(hsiir_rbuf,0,sizeof(hsiir_rbuf));
    memset(tpl_lbuf,0,sizeof(tpl_lbuf));
    memset(tpl_rbuf,0,sizeof(tpl_rbuf));
}

static float hsiir_rss(float *pbuf, long nsamp)
{
    float rss_sum;
    int   ix;

    rss_sum = 0.0f;
    for(ix = nsamp; --ix >= 0;)
    {
        float yl, zl, yr, zr;

        yl = pbuf[0] * hsiir_coffs[0] +
                    hsiir_lbuf[0] * hsiir_coffs[1] +
                    hsiir_lbuf[1] * hsiir_coffs[2] +
                    hsiir_lbuf[2] * hsiir_coffs[3] +
                    hsiir_lbuf[3] * hsiir_coffs[4];
        zl = yl * hsiir_coffs[5] +
                    hsiir_lbuf[2] * hsiir_coffs[6] +
                    hsiir_lbuf[3] * hsiir_coffs[7] +
                    hsiir_lbuf[4] * hsiir_coffs[8] +
                    hsiir_lbuf[5] * hsiir_coffs[9];
        hsiir_lbuf[1] = hsiir_lbuf[0];
        hsiir_lbuf[0] = pbuf[0];
        hsiir_lbuf[3] = hsiir_lbuf[2];
        hsiir_lbuf[2] = yl;
        hsiir_lbuf[5] = hsiir_lbuf[4];
        hsiir_lbuf[4] = zl;
        
        yr = pbuf[1] * hsiir_coffs[0] +
                    hsiir_rbuf[0] * hsiir_coffs[1] +
                    hsiir_rbuf[1] * hsiir_coffs[2] +
                    hsiir_rbuf[2] * hsiir_coffs[3] +
                    hsiir_rbuf[3] * hsiir_coffs[4];
        zr = yr * hsiir_coffs[5] +
                    hsiir_rbuf[2] * hsiir_coffs[6] +
                    hsiir_rbuf[3] * hsiir_coffs[7] +
                    hsiir_rbuf[4] * hsiir_coffs[8] +
                    hsiir_rbuf[5] * hsiir_coffs[9];
        hsiir_rbuf[1] = hsiir_rbuf[0];
        hsiir_rbuf[0] = pbuf[1];
        hsiir_rbuf[3] = hsiir_rbuf[2];
        hsiir_rbuf[2] = yr;
        hsiir_rbuf[5] = hsiir_rbuf[4];
        hsiir_rbuf[4] = zr;
        
        rss_sum += zl * zl + zr * zr;
        pbuf += 2;
    }
    return (rss_sum / nsamp);
}

extern "C"
void hsiir_eval_blk(float *pbuf, long nsamp, float *pdst)
{
    pdst[1] = hsiir_tpl(pbuf, nsamp);
    pdst[0] = hsiir_rss(pbuf, nsamp);
}

