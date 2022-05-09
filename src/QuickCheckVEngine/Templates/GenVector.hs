--
-- SPDX-License-Identifier: BSD-2-Clause
--
-- Copyright (c) 2019 Peter Rugg
-- Copyright (c) 2019, 2020 Alexandre Joannou
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

module QuickCheckVEngine.Templates.GenVector (
  gen_rv64_v_arithmetic
) where

import InstrCodec
import RISCV.RV64_I
import Test.QuickCheck
import RISCV.RV64_V
import RISCV.RV_CSRs
import QuickCheckVEngine.Template
import QuickCheckVEngine.Templates.Utils
import RISCV
import Data.Bits

gen_rv64_v_arithmetic :: Template
gen_rv64_v_arithmetic = random $ do
  imm           <- bits 5
  vs1           <- src
  vs2           <- src
  rs1           <- src
  addrReg       <- src
  tmpReg        <- src
  vd            <- dest
  sew           <- choose (0, 2) :: Gen Int
  w             <- choose (3, 3) :: Gen Int -- Set to (3,3) for basic rvv-Flute, (2,4) for 128 version
  arith_index   <- choose (0, 11) :: Gen Int
  rnd_num       <- choose (0, 65535) :: Gen Int
  rnd_shift     <- choose (0, 48) :: Gen Int
  let def = 1 in return $ dist [
                  (100 * def, instSeq [  -- Do arithmetic and check destination
                    addi rs1 rs1 0,
                    addi 12 0 1,
                    slli 12 12 31,
                    addi 12 12 8,
                    addi 12 12 8,        -- Generate address
                    vstore vs1 12,       -- Store and check vs1 matches
                    ld 13 12 0,          -- 0 offset
                    ld 13 12 8,          -- 8 offset
                    vstore vs2 12,       -- Store and check vs2 matches
                    ld 13 12 0,          -- 0 offset
                    ld 13 12 8,          -- 8 offset
                    vstore vd 12,        -- Store and check vd matches
                    ld 13 12 0,          -- 0 offset
                    ld 13 12 8,          -- 8 offset
                    addi rs1 rs1 0,      -- Check rs1 matches
                    (rv64_v_arith vs1 vs2 vd imm rs1)!!arith_index,
                    vstore vd 12,        -- Result into GPR
                    ld 13 12 0,          -- compare at  0 offset
                    ld 13 12 8,          -- 64 bit offset
                    ld 13 12 16          -- 128 bit offset
                     ]),
                  (1 * def, noShrink (instSeq [ -- Setup vector environment for 8 bit   --noshrink decorator
                    addi 12 0 11,
                    slli 12 12 0x17,
                    csrrs 0 (unsafe_csrs_indexFromName "mstatus" ) 12,
                    addi 14 0 (shift (  (shift 1 w)  :: Integer) (-sew)),
                    vsetvli (toInteger sew) 14 12,              -- Setup vector environment
                      addi 12 0 1,    -- Generate address
                      slli 12 12 31,
                      addi 12 12 8,
                      addi 12 12 8,
                    addi 13 0 0,         -- Generate dummy value
                    sd 12 13 0,      -- Store dummy at address
                    vload 0 12,
                    vload 1 12,
                    vload 2 12, 
                    vload 3 12,
                    vload 4 12,
                    vload 5 12,
                    vload 6 12,
                    vload 7 12,
                    vload 8 12,
                    vload 9 12,
                    vload 10 12,
                    vload 11 12,
                    vload 12 12,
                    vload 13 12,
                    vload 14 12,
                    vload 15 12,
                    vload 16 12,
                    vload 17 12,
                    vload 18 12,
                    vload 19 12,
                    vload 20 12,
                    vload 21 12,
                    vload 22 12,
                    vload 23 12,
                    vload 24 12,
                    vload 25 12,
                    vload 26 12,
                    vload 27 12,
                    vload 28 12,
                    vload 29 12,
                    vload 30 12,
                    vload 31 12
                     ])),
                  (1 * def, instSeq [ -- Load random
                    addi 12 0 1,
                    slli 12 12 31,
                    addi 12 12 8,
                    addi 12 12 8,
                    addi rs1 0 (toInteger rnd_num),     
                    slli rs1 rs1 (toInteger rnd_shift), -- Set rs1 to a random value
                    sd 12 rs1 0,                        -- Store dummy at address
                    vload vs1 12                        -- Load dummy into vector reg
                     ]),
                  (1 * def, instSeq [ -- Store and check random
                    addi 12 0 1,
                    slli 12 12 31,
                    addi 12 12 8,
                    addi 12 12 8,  -- Generate address  
                    vstore vs1 12, -- Store rnd vec in gpr
                    ld 13 12 0,    -- Load first 64 bit for comparison
                    ld 13 12 8     -- Load second 64 bit for comparison
                     ])
                ]
