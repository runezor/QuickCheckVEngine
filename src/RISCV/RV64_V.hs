--
-- SPDX-License-Identifier: BSD-2-Clause
--
-- Copyright (c) 2019-2020 Alexandre Joannou
-- Copyright (c) 2020 Peter Rugg
-- All rights reserved.
--
-- This software was developed by SRI International and the University of
-- Cambridge Computer Laboratory (Department of Computer Science and
-- Technology) under DARPA contract HR0011-18-C-0016 ("ECATS"), as part of the
-- DARPA SSITH research programme.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
--
-- THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
-- ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
-- ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
-- OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
-- LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
-- OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
-- SUCH DAMAGE.
--

{-|
    Module      : RISCV.RV64_A
    Description : RISC-V RV64 atomic extension

    The 'RISCV.RV64_A' module provides the description of the RISC-V RV64 Atomic
    extension
-}

module RISCV.RV64_V (
-- * RV64 vector, instruction definitions
  vadd_vv,
  vsetvli,
  vadd_vi,
  vsub_vi,
  vadd_vv,
  vstore,
  vload,
-- * RV64 vector, others
 rv64_v_arith,
 rv64_v_disass,
) where

import RISCV.Helpers (pretty_V_VVop, pretty_V_VIop, pretty_V_VXop, pretty_V_Mem, pretty_V_Vset)
import InstrCodec (DecodeBranch, (-->), encode, Instruction)

-- We set VM=1 for no vector masking
vadd_vv_raw               =                     "000000 1 vs2[4:0] vs1[4:0]  000 vd[4:0] 1010111"
vadd_vv    vd vs1 vs2     = encode vadd_vv_raw            vs2      vs1           vd       
vsub_vv_raw               =                     "000010 1 vs2[4:0] vs1[4:0]  000 vd[4:0] 1010111"
vsub_vv    vd vs1 vs2     = encode vsub_vv_raw            vs2      vs1           vd           
vmul_vv_raw               =                     "100101 1 vs2[4:0] vs1[4:0]  010 vd[4:0] 1010111" --OPMVV
vmul_vv    vd vs1 vs2     = encode vmul_vv_raw            vs2      vs1           vd       
vsaddu_vv_raw             =                     "100000 1 vs2[4:0] vs1[4:0]  000 vd[4:0] 1010111"
vsaddu_vv   vd vs1 vs2    = encode vsaddu_vv_raw          vs2      vs1           vd          
vssubu_vv_raw             =                     "100010 1 vs2[4:0] vs1[4:0]  000 vd[4:0] 1010111"
vssubu_vv    vd vs1 vs2   = encode vssubu_vv_raw          vs2      vs1           vd       

vadd_vi_raw               =                     "000000 1 vs2[4:0] imm[4:0]  011 vd[4:0] 1010111"
vadd_vi    vd imm vs2     = encode vadd_vi_raw            vs2      imm           vd
vsub_vi_raw               =                     "000010 1 vs2[4:0] imm[4:0]  011 vd[4:0] 1010111"
vsub_vi    vd imm vs2     = encode vsub_vi_raw            vs2      imm           vd           
vmul_vi_raw               =                     "100101 1 vs2[4:0] imm[4:0]  011 vd[4:0] 1010111"
vmul_vi    vd imm vs2     = encode vmul_vi_raw            vs2      imm           vd       
vsaddu_vi_raw             =                     "100000 1 vs2[4:0] imm[4:0]  011 vd[4:0] 1010111"
vsaddu_vi    vd imm vs2   = encode vsaddu_vi_raw          vs2      imm           vd          
vssubu_vi_raw             =                     "100010 1 vs2[4:0] imm[4:0]  011 vd[4:0] 1010111"
vssubu_vi    vd imm vs2   = encode vssubu_vi_raw          vs2      imm           vd       

vadd_vx_raw               =                      "000000 1 vs2[4:0] rs1[4:0]  100 vd[4:0] 1010111"
vadd_vx    vd rs1 vs2     = encode vadd_vx_raw             vs2      rs1           vd
vsub_vx_raw               =                      "000010 1 vs2[4:0] rs1[4:0]  100 vd[4:0] 1010111"
vsub_vx    vd rs1 vs2     = encode vsub_vx_raw             vs2      rs1           vd           
vmul_vx_raw               =                      "100101 1 vs2[4:0] rs1[4:0]  110 vd[4:0] 1010111" --OPMVX
vmul_vx    vd rs1 vs2     = encode vmul_vx_raw             vs2      rs1           vd       
vsaddu_vx_raw             =                      "100000 1 vs2[4:0] rs1[4:0]  100 vd[4:0] 1010111"
vsaddu_vx    vd rs1 vs2   = encode vsaddu_vx_raw           vs2      rs1           vd          
vssubu_vx_raw             =                      "100010 1 vs2[4:0] rs1[4:0]  100 vd[4:0] 1010111"
vssubu_vx   vd rs1 vs2    = encode vssubu_vx_raw           vs2      rs1           vd       

-- mop: memory addressing mode, nf: number of fields, sumop: variant.
-- "nf mop 1 sumop rs1[4:0]  width vd[4:0] 000111"
vstore_raw              =                      "000 000 1 00000 rs1[4:0]  111 vs3[4:0] 0100111"
vstore    vs3 rs1       = encode vstore_raw                     rs1           vs3
-- mop: memory addressing mode, nf: number of fields, sumop: variant, width: 111
-- "nf mop 1 sumop rs1[4:0]  width vd[4:0] 000111"
vload_raw              =                      "000 000 1 00000 rs1[4:0]  111 vd[4:0] 0000111"
vload    vd rs1        = encode vload_raw                     rs1           vd


-- Proposed vtype: vsew = 001, SEW:  
-- 0000 00 001 00
-- ???? ?? SEW lmul
vsetvli_raw               =                     "0 000000 vsew[2:0] 00 rs1[4:0] 111 rd[4:0] 1010111"
vsetvli vsew rs1 rd       = encode vsetvli_raw            vsew         rs1          rd    

-- | Dissassembly of RV64 vector instructions
rv64_v_disass :: [DecodeBranch String]
rv64_v_disass = [ vadd_vv_raw    --> pretty_V_VVop     "vadd_vv"
                , vsub_vv_raw    --> pretty_V_VVop     "vsub_vv"
                , vmul_vv_raw    --> pretty_V_VVop     "vmul_vv"
                , vsaddu_vv_raw  --> pretty_V_VVop     "vsaddu_vv"
                , vssubu_vv_raw  --> pretty_V_VVop     "vssubu_vv"
                , vadd_vi_raw    --> pretty_V_VIop     "vadd_vi"
                , vsub_vi_raw    --> pretty_V_VIop     "vsub_vi"
                , vmul_vi_raw    --> pretty_V_VIop     "vmul_vi"
                , vsaddu_vi_raw  --> pretty_V_VIop     "vsaddu_vi"
                , vssubu_vi_raw  --> pretty_V_VIop     "vssubu_vi"
                , vadd_vx_raw    --> pretty_V_VXop     "vadd_vx"
                , vsub_vx_raw    --> pretty_V_VXop     "vsub_vx"
                , vmul_vx_raw    --> pretty_V_VXop     "vmul_vx"
                , vsaddu_vx_raw  --> pretty_V_VXop     "vsaddu_vx"
                , vssubu_vx_raw  --> pretty_V_VXop     "vssubu_vx"
                , vstore_raw     --> pretty_V_Mem      "vstore"
                , vload_raw      --> pretty_V_Mem      "vload"
                , vsetvli_raw    --> pretty_V_Vset     "vsetvli"
                 ]

-- | List of RV64 vector arithmetic instructions
rv64_v_arith :: Integer -> Integer -> Integer -> Integer -> Integer -> [Instruction]
rv64_v_arith vs1 vs2 vd imm rs1 = [ vadd_vv      vd vs1 vs2
                                  ,   vsub_vv    vd vs1 vs2
                                  ,   vmul_vv    vd vs1 vs2
                                  ,   vsaddu_vv   vd vs1 vs2
                                  ,   vssubu_vv   vd vs1 vs2
                                  ,   vadd_vi    vd imm vs2
                                  --,   vsub_vi    vd imm vs2
                                  --,   vmul_vi    vd imm vs2 POORLY FORMED?
                                  ,   vsaddu_vi   vd imm vs2
                                  --,   vssub_vi   vd imm vs2
                                  ,   vadd_vx    vd rs1 vs2
                                  ,   vsub_vx    vd rs1 vs2
                                  ,   vmul_vx    vd rs1 vs2
                                  ,   vsaddu_vx   vd rs1 vs2
                                  ,   vssubu_vx   vd rs1 vs2
                                  ]