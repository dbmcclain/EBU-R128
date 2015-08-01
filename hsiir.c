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

#if 1
// --------------------------------------------------
// Vectorized Version...

typedef union {
   __m128 v;
   float  f[4];
} f4vector;

static long     tpl_ix = 0;
static f4vector tplvl_state[24];
static f4vector tplvr_state[24];
static f4vector tplv_coffs[12];

static void tplv_init()
{
  for(long ix = 12; --ix >= 0; )
    {
      tplv_coffs[ix].f[0] = tpl_ph0[ix];
      tplv_coffs[ix].f[1] = tpl_ph1[ix];
      tplv_coffs[ix].f[2] = tpl_ph2[ix];
      tplv_coffs[ix].f[3] = tpl_ph3[ix];
    }
    tpl_ix = 0;
}

static void save_firv_sample(float vl, float vr)
{
   if(--tpl_ix < 0)
     tpl_ix = 11;

   tplvl_state[tpl_ix].f[0] = vl;
   tplvl_state[tpl_ix].f[1] = vl;
   tplvl_state[tpl_ix].f[2] = vl;
   tplvl_state[tpl_ix].f[3] = vl;
   tplvl_state[tpl_ix+12].f[0] = vl;
   tplvl_state[tpl_ix+12].f[1] = vl;
   tplvl_state[tpl_ix+12].f[2] = vl;
   tplvl_state[tpl_ix+12].f[3] = vl;

   tplvr_state[tpl_ix].f[0] = vr;
   tplvr_state[tpl_ix].f[1] = vr;
   tplvr_state[tpl_ix].f[2] = vr;
   tplvr_state[tpl_ix].f[3] = vr;
   tplvr_state[tpl_ix+12].f[0] = vr;
   tplvr_state[tpl_ix+12].f[1] = vr;
   tplvr_state[tpl_ix+12].f[2] = vr;
   tplvr_state[tpl_ix+12].f[3] = vr;
}

static double tplv_maxsqr()
{
  f4vector *ph = tplv_coffs;
  
  f4vector *psl = &tplvl_state[tpl_ix];
  f4vector suml;
  suml.v = _mm_mul_ps(ph[0].v,psl[0].v);
  suml.v = _mm_add_ps(suml.v, _mm_mul_ps(ph[1].v,psl[1].v));
  suml.v = _mm_add_ps(suml.v, _mm_mul_ps(ph[2].v,psl[2].v));
  suml.v = _mm_add_ps(suml.v, _mm_mul_ps(ph[3].v,psl[3].v));
  suml.v = _mm_add_ps(suml.v, _mm_mul_ps(ph[4].v,psl[4].v));
  suml.v = _mm_add_ps(suml.v, _mm_mul_ps(ph[5].v,psl[5].v));
  suml.v = _mm_add_ps(suml.v, _mm_mul_ps(ph[6].v,psl[6].v));
  suml.v = _mm_add_ps(suml.v, _mm_mul_ps(ph[7].v,psl[7].v));
  suml.v = _mm_add_ps(suml.v, _mm_mul_ps(ph[8].v,psl[8].v));
  suml.v = _mm_add_ps(suml.v, _mm_mul_ps(ph[9].v,psl[9].v));
  suml.v = _mm_add_ps(suml.v, _mm_mul_ps(ph[10].v,psl[10].v));
  suml.v = _mm_add_ps(suml.v, _mm_mul_ps(ph[11].v,psl[11].v));
  suml.v = _mm_mul_ps(suml.v, suml.v);

  f4vector sumr;
  f4vector *psr = &tplvr_state[tpl_ix];
  sumr.v = _mm_mul_ps(ph[0].v,psr[0].v);
  sumr.v = _mm_add_ps(sumr.v, _mm_mul_ps(ph[1].v,psr[1].v));
  sumr.v = _mm_add_ps(sumr.v, _mm_mul_ps(ph[2].v,psr[2].v));
  sumr.v = _mm_add_ps(sumr.v, _mm_mul_ps(ph[3].v,psr[3].v));
  sumr.v = _mm_add_ps(sumr.v, _mm_mul_ps(ph[4].v,psr[4].v));
  sumr.v = _mm_add_ps(sumr.v, _mm_mul_ps(ph[5].v,psr[5].v));
  sumr.v = _mm_add_ps(sumr.v, _mm_mul_ps(ph[6].v,psr[6].v));
  sumr.v = _mm_add_ps(sumr.v, _mm_mul_ps(ph[7].v,psr[7].v));
  sumr.v = _mm_add_ps(sumr.v, _mm_mul_ps(ph[8].v,psr[8].v));
  sumr.v = _mm_add_ps(sumr.v, _mm_mul_ps(ph[9].v,psr[9].v));
  sumr.v = _mm_add_ps(sumr.v, _mm_mul_ps(ph[10].v,psr[10].v));
  sumr.v = _mm_add_ps(sumr.v, _mm_mul_ps(ph[11].v,psr[11].v));
  sumr.v = _mm_mul_ps(sumr.v, sumr.v);

  suml.v = _mm_max_ps(suml.v, sumr.v);
  
  float tpl = suml.f[0];
  if(suml.f[1] > tpl)
    tpl = suml.f[1];
  if(suml.f[2] > tpl)
    tpl = suml.f[2];
  if(suml.f[3] > tpl)
    tpl = suml.f[3];
  return tpl;
}

extern "C"
float hsiirv_tpl(float* pbuf, long nsamp)
{
    float tpl_max = 0.0f;
    
    while(--nsamp >= 0)
    {
        float tpl;

        save_firv_sample(pbuf[0], pbuf[1]);
        pbuf += 2;
        tpl = tplv_maxsqr();
        if(tpl > tpl_max)
            tpl_max = tpl;
    }
    return tpl_max;
}

static f4vector hsiir_coffs[10];
static f4vector hsiir_state[6];

static float hsiirv_rss(float *pbuf, long nsamp)
{
    float rss_sum;
    long  ix;
    f4vector x, y, z;

    x.f[2] = 0.0f; // just to avoid any slowdown from possible NaN's
    x.f[3] = 0.0f;
    rss_sum = 0.0f;
    
    for(ix = nsamp; --ix >= 0;)
    {
      x.f[0] = pbuf[0];
      x.f[1] = pbuf[1];
      pbuf += 2;
      
      y.v = _mm_mul_ps(x.v, hsiir_coffs[0].v);
      y.v = _mm_add_ps(y.v, _mm_mul_ps(hsiir_state[0].v, hsiir_coffs[1].v));
      y.v = _mm_add_ps(y.v, _mm_mul_ps(hsiir_state[1].v, hsiir_coffs[2].v));
      y.v = _mm_add_ps(y.v, _mm_mul_ps(hsiir_state[2].v, hsiir_coffs[3].v));
      y.v = _mm_add_ps(y.v, _mm_mul_ps(hsiir_state[3].v, hsiir_coffs[4].v));
      
      z.v = _mm_mul_ps(y.v, hsiir_coffs[5].v);
      z.v = _mm_add_ps(z.v, _mm_mul_ps(hsiir_state[2].v, hsiir_coffs[6].v));
      z.v = _mm_add_ps(z.v, _mm_mul_ps(hsiir_state[3].v, hsiir_coffs[7].v));
      z.v = _mm_add_ps(z.v, _mm_mul_ps(hsiir_state[4].v, hsiir_coffs[8].v));
      z.v = _mm_add_ps(z.v, _mm_mul_ps(hsiir_state[5].v, hsiir_coffs[9].v));

      hsiir_state[1].v = hsiir_state[0].v;
      hsiir_state[0].v = x.v;

      hsiir_state[3].v = hsiir_state[2].v;
      hsiir_state[2].v = y.v;

      hsiir_state[5].v = hsiir_state[4].v;
      hsiir_state[4].v = z.v;

      z.v = _mm_mul_ps(z.v, z.v);
      rss_sum += z.f[0] + z.f[1];
    }
    return (rss_sum / nsamp);
}

extern "C"
void hsiir_init(float *pcoffs)
{
  memset(hsiir_coffs,0,sizeof(hsiir_coffs)); // clear out to avoid NaN's
  memset(hsiir_state,0,sizeof(hsiir_state));
  tplv_init();
  for(long ix = 10; --ix >= 0; )
    {
      hsiir_coffs[ix].f[0] = pcoffs[ix];
      hsiir_coffs[ix].f[1] = pcoffs[ix];
    }
}

extern "C"
void hsiir_eval_blk(float *pbuf, long nsamp, float *pdst)
{
    pdst[1] = hsiirv_tpl(pbuf, nsamp);
    pdst[0] = hsiirv_rss(pbuf, nsamp);
}

#else
// ----------------------------------------------------------------
// Non-Vectorized Version...

static long  tpl_ix = 0;
static float tpl_lbuf[24];
static float tpl_rbuf[24];

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
    long ix;
    float suml, sumr;
    float *pl, *pr;

    suml = 0.0f;
    sumr = 0.0f;
    pl = &tpl_lbuf[tpl_ix];
    pr = &tpl_rbuf[tpl_ix];
    for(ix = 12; --ix >= 0;)
    {
       suml += *ph * *pl++;
       sumr += *ph * *pr++;
       ++ph;
    }  
    suml *= suml;
    sumr *= sumr;
    return (suml > sumr ? suml : sumr);
}

static float tpl_maxsqr()
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
        tpl = tpl_maxsqr();
        if(tpl > tpl_max)
            tpl_max = tpl;
    }
    return tpl_max;
}

static float hsiir_coffs[10];
static float hsiir_lbuf[6];
static float hsiir_rbuf[6];

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
void hsiir_init(float *pcoffs)
{
    memcpy(hsiir_coffs, pcoffs, 10*sizeof(float));
    memset(hsiir_lbuf,0,sizeof(hsiir_lbuf));
    memset(hsiir_rbuf,0,sizeof(hsiir_rbuf));
    memset(tpl_lbuf,0,sizeof(tpl_lbuf));
    memset(tpl_rbuf,0,sizeof(tpl_rbuf));
    tpl_ix = 0;
}

extern "C"
void hsiir_eval_blk(float *pbuf, long nsamp, float *pdst)
{
    pdst[1] = hsiir_tpl(pbuf, nsamp);
    pdst[0] = hsiir_rss(pbuf, nsamp);
}

#endif
