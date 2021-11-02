--
-- SPDX-License-Identifier: BSD-2-Clause
--
-- Copyright (c) 2019-2020 Peter Rugg
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

module QuickCheckVEngine.Templates.GenMemory (
  gen_rv32_i_memory
, gen_rv32_i_a_memory
, gen_rv32_i_zifencei_memory
, gen_rv32_i_a_zifencei_memory
, gen_rv32_i_cache
, gen_rv64_i_memory
, gen_rv64_i_a_memory
, gen_rv64_i_zifencei_memory
, gen_rv64_i_a_zifencei_memory
, gen_rv64_i_cache
, gen_rv32_Xcheri_cache
, gen_rv64_Xcheri_cache
, gen_rv32_zifencei_Xcheri_cache
, gen_rv64_zifencei_Xcheri_cache
, gen_pte_perms
, gen_pte_trans_core
, gen_pte_trans
) where

import InstrCodec
import Test.QuickCheck
import RISCV.RV32_I
import RISCV.RV32_A
import RISCV.RV32_Zifencei
import RISCV.RV32_Zicsr
import RISCV.RV64_I
import RISCV.RV32_Xcheri
import QuickCheckVEngine.Template
import QuickCheckVEngine.Templates.Utils
import Data.Bits

data GenConf = GenConf { has_a        :: Bool
                       , has_zifencei :: Bool
                       , has_xlen_64  :: Bool
                       }

gen_rv32_i_memory :: Template
gen_rv32_i_memory = gen_memory False False False False

gen_rv32_i_a_memory :: Template
gen_rv32_i_a_memory = gen_memory True False False False

gen_rv32_i_zifencei_memory :: Template
gen_rv32_i_zifencei_memory = gen_memory False True False False

gen_rv32_i_a_zifencei_memory :: Template
gen_rv32_i_a_zifencei_memory = gen_memory True True False False

gen_rv32_i_cache :: Template
gen_rv32_i_cache = gen_cache False True False False

gen_rv64_i_memory :: Template
gen_rv64_i_memory = gen_memory False False True False

gen_rv64_i_a_memory :: Template
gen_rv64_i_a_memory = gen_memory True False True False

gen_rv64_i_zifencei_memory :: Template
gen_rv64_i_zifencei_memory = gen_memory False True True False

gen_rv64_i_a_zifencei_memory :: Template
gen_rv64_i_a_zifencei_memory = gen_memory True True True False

gen_rv64_i_cache :: Template
gen_rv64_i_cache = gen_cache False True True False

gen_rv32_Xcheri_cache :: Template
gen_rv32_Xcheri_cache = gen_cache False False False True

gen_rv64_Xcheri_cache :: Template
gen_rv64_Xcheri_cache = gen_cache False False True True

gen_rv32_zifencei_Xcheri_cache :: Template
gen_rv32_zifencei_Xcheri_cache = gen_cache False True False True

gen_rv64_zifencei_Xcheri_cache :: Template
gen_rv64_zifencei_Xcheri_cache = gen_cache False True True True

gen_memory :: Bool -> Bool -> Bool -> Bool -> Template
gen_memory has_a has_zifencei has_xlen_64 has_caplen = Random $
  do imm      <- bits 12
     src1     <- src
     src2     <- src
     dest     <- dest
     fenceOp1 <- bits 4
     fenceOp2 <- bits 4
     aq       <- bits 1
     rl       <- bits 1
     offset   <- geomBits 11 0
     let insts = [[(8,  Single $ addi dest src1 offset)
                 , (8,  Single $ ori dest src1 offset)
                 , (16, instSeq [ lui 0x40004 dest
                                , slli dest dest 1 ])
                 , (8, uniformTemplate $ rv32_i_load  src1 dest offset)
                 , (8, uniformTemplate $ rv32_i_store src1 src2 offset)
                 , (2, uniformTemplate $ rv32_i_fence fenceOp1 fenceOp2)]]
                 ++ [[(2, uniformTemplate $ rv32_a src1 src2 dest aq rl)] | has_a]
                 ++ [[(2,  Single fence_i)] | has_zifencei]
                 ++ [[(8, uniformTemplate $ rv64_i_load  src1 dest offset)
                    , (8, uniformTemplate $ rv64_i_store src1 src2 offset)] | has_xlen_64]
     return $ Distribution $ concat insts

gen_cache :: Bool -> Bool -> Bool -> Bool -> Template
gen_cache has_a has_zifencei has_xlen_64 has_caplen = Random $
  do size     <- getSize
     src1     <- elements [1, 2, 3]
     src2     <- elements [1, 2, 3]
     dest     <- elements [1, 2, 3]

     aq       <- bits 1
     rl       <- bits 1
     offseta  <- elements [0, 8, 16, 64]
     offsetb  <- elements [0, 8, 16, 64]
     offsetc  <- elements [0, 8, 16, 64]
     upperIa  <- elements [0x40000, 0x40080, 0x40100, 0x40180, 0x40200, 0x40280]
     upperIb  <- elements [0x40000, 0x40080, 0x40100, 0x40180, 0x40200, 0x40280]
     upperIc  <- elements [0x40000, 0x40080, 0x40100, 0x40180, 0x40200, 0x40280]
     let prologue_list = [ lui 1 upperIa
                         , slli 1 1 1
                         , addi 1 1 offseta
                         , lui 2 upperIb
                         , slli 2 2 1
                         , addi 2 2 offsetb
                         , lui 3 upperIc
                         , slli 3 3 1
                         , addi 3 3 offsetc]
     let insts = [[ (8, Random $ do src <- elements [1, 2, 3]
                                    return $ uniformTemplate $ rv32_i_load  src 13 0)
                  , (8, Random $ do src1 <- elements [1, 2, 3]
                                    src2 <- elements [1, 2, 3]
                                    return $ uniformTemplate $ rv32_i_store src1 src2 0)
                  , (2, Random $ do fenceOp1 <- bits 4
                                    fenceOp2 <- bits 4
                                    return $ uniformTemplate $ rv32_i_fence fenceOp1 fenceOp2)]]
              ++ [[(2, uniformTemplate $ rv32_a src1 src2 dest aq rl)] | has_a]
              ++ [[(2, Single fence_i)] | has_zifencei]
              ++ [[(2, Random $ do src <- elements [1, 2, 3]
                                   return $ uniformTemplate $ rv64_i_load  src 13 0)
                  ,(2, Random $ do src1 <- elements [1, 2, 3]
                                   src2 <- elements [1, 2, 3]
                                   return $ uniformTemplate $ rv64_i_store src1 src2 0)] | has_xlen_64]
              ++ [[(2, Random $ do srcAddr <- elements [1, 2, 3]
                                   return $ instSeq [ (if has_xlen_64 then lq else ld) 13 srcAddr 0,
                                                      cgettag 13 13])
                  ,(2, Random $ do srcData <- elements [1, 2, 3, 4, 5]
                                   srcAddr <- elements [1, 2, 3]
                                   return $ Single $ (if has_xlen_64 then sq else sd) srcAddr srcData 0)] | has_caplen]
     let prologue = instSeq prologue_list
     return $ prologue
              <> replicateTemplate (size - length prologue_list - 1)
                                   (Distribution $ concat insts)


gen_pte_perms = Random $
  do lperms <- bits 10
     uperms <- bits 5
     clg0 <- bits 2
     clg1 <- bits 2
     return $ Sequence [Single $ ori 1 0 uperms, -- two cheri pte bits
                        Single $ slli 1 1 16,
                        Single $ ori 1 1 0x000, -- 11 msbs of PA
                        Single $ slli 1 1 11,
                        Single $ ori 1 1 0x000, -- next 11 bits of PA
                        Single $ slli 1 1 11,
                        Single $ ori 1 1 0x100, -- next 11 bits of PA
                        Single $ slli 1 1 11,
                        Single $ ori 1 1 0x000, -- next 11 bits of PA
                        Single $ slli 1 1 10,
                        Single $ ori 1 1 lperms, -- Permissions bits
                        Single $ addi 5 0 1,
                        Single $ slli 5 5 31,
                        Single $ slli 5 5 31,
                        Single $ slli 5 5 1,
                        Single $ lui 6 0x80,
                        Single $ add 5 5 6,
                        Single $ lui 7 0x40000,
                        Single $ slli 7 7 1,
                        Single $ sd 7 1 0,
                        Single $ csrrw 0 0x180 5,
                        Single $ csrrwi 0 0x9c0 (clg0 * 4)]
                        <>
                        (NoShrink $ Single $ sfence 0 0)
                        <> Sequence [
                        Single sret,
                        Distribution [(1,Single $ ccleartag 3 3), (1,Single $ cmove 3 3)],
                        Distribution [(1,Single $ sw 0 3 16), (1,Single $ sq 0 3 16)],
                        Distribution [(1,Single $ lw 4 0 16), (1,Single $ lq 4 0 16)],
                        Single $ csrrwi 0 0x9c0 (clg1 * 4),
                        Distribution [(1,Single $ lw 4 0 16), (1,Single $ lq 4 0 16)],
                        Single $ cgettag 5 4,
                        Single ecall]

gen_pte_trans_core lxReg addrReg pteReg = Random $
  do let leafperms = 0xFF
     let parentperms = 0xF1
     l2perms <- elements [leafperms, parentperms]
     l1perms <- elements [leafperms, parentperms]
     l0perms <- elements [leafperms, parentperms]
     let satpa = 0x803ff000
     let l2pa  = 0x80000000
     let l1pa  = 0x80200000
     let l0pa  = 0x80001000
     let satp = (shiftL 0x8 60) + (shiftR satpa 12)
     let l2pte = (shiftR l2pa 2) + l2perms
     let l1pte = (shiftR l1pa 2) + l1perms
     let l0pte = (shiftR l0pa 2) + l0perms
     addrInitial <- elements [0x00000000, 0x00000800, 0x00100000]
     return $ Sequence [li64 pteReg l2pte,
                        li64 lxReg  satpa,
                        Single $ sd lxReg pteReg 0,
                        li64 pteReg l1pte,
                        li64 lxReg  l2pa,
                        Single $ sd lxReg pteReg 0,
                        li64 pteReg l0pte,
                        li64 lxReg  l1pa,
                        Single $ sd lxReg pteReg 0,
                        li64 pteReg satp,
                        Single $ csrrw 0 0x180 pteReg, -- SATP write
                        li64 addrReg addrInitial]
                        <>
                        (NoShrink $ Single $ sfence 0 0)
                        <>
                        (Single sret)



gen_pte_trans = Random $
  do let lxreg = 1
     let addrReg = 2
     let ptereg = 30
     return $ (gen_pte_trans_core lxreg addrReg ptereg)
               <>
               (replicateTemplate 20
                                 (Random $
                                  do imm <- elements [0x10, 0x100, 0x0]
                                     datReg <- elements [ptereg, lxreg]
                                     return $ Distribution [(8, uniformTemplate $ rv64_i_load  addrReg datReg imm),
                                                            (8, uniformTemplate $ rv64_i_store addrReg datReg imm),
                                                            (4, Single $ addi addrReg addrReg imm),
                                                            (1, Single $ fence_i),
                                                            (1, Single $ fence 0 0)]))
               <>
               (NoShrink $ Single ecall)
