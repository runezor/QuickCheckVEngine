--
-- SPDX-License-Identifier: BSD-2-Clause
--
-- Copyright (c) 2019 Alexandre Joannou
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

module GenMulDiv where

import InstrCodec
import Test.QuickCheck
import ISA_Helpers
import RVxxM
import Template
import Prelude hiding (rem, div)

genMulDiv :: Template
genMulDiv = Random $ do
  src1     <- src;
  src2     <- src;
  dest     <- dest;
  return $ uniform [ encode mul    src1 src2 dest
                   , encode mulh   src1 src2 dest
                   , encode mulhsu src1 src2 dest
                   , encode mulhu  src1 src2 dest
                   , encode div    src1 src2 dest
                   , encode divu   src1 src2 dest
                   , encode rem    src1 src2 dest
                   , encode remu   src1 src2 dest
                   ]

genMulDiv64 :: Template
genMulDiv64 = Random $ do
  src1     <- src;
  src2     <- src;
  dest     <- dest;
  return $ uniform [ encode mulw  src1 src2 dest
                   , encode divw  src1 src2 dest
                   , encode divuw src1 src2 dest
                   , encode remw  src1 src2 dest
                   , encode remuw src1 src2 dest
                   ]