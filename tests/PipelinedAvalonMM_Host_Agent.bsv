/*-
 * Copyright (c) 2023 Alexandre Joannou
 * All rights reserved.
 *
 * This material is based upon work supported by the DoD Information Analysis
 * Center Program Management Office (DoD IAC PMO), sponsored by the Defense
 * Technical Information Center (DTIC) under Contract No. FA807518D0004.  Any
 * opinions, findings and conclusions or recommendations expressed in this
 * material are those of the author(s) and do not necessarily reflect the views
 * of the Air Force Installation Contracting Agency (AFICA).
 *
 * @BERI_LICENSE_HEADER_START@
 *
 * Licensed to BERI Open Systems C.I.C. (BERI) under one or more contributor
 * license agreements.  See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.  BERI licenses this
 * file to you under the BERI Hardware-Software License, Version 1.0 (the
 * "License"); you may not use this file except in compliance with the
 * License.  You may obtain a copy of the License at:
 *
 *   http://www.beri-open-systems.org/legal/license-1-0.txt
 *
 * Unless required by applicable law or agreed to in writing, Work distributed
 * under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
 * CONDITIONS OF ANY KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations under the License.
 *
 * @BERI_LICENSE_HEADER_END@
 */

package PipelinedAvalonMM_Host_Agent;

import BlueBasics :: *;
import AvalonMemoryMapped :: *;

import Connectable :: *;
import FIFOF :: *;
import SpecialFIFOs :: *;

`define ADDR_W 32
`define DATA_W 32
`define DELAY  5

Integer nb_transaction = 5;

function Action tprint (Fmt msg) = $display ("<%0t> ", $time, msg);

module mkPipelinedAvalonMMHost (PipelinedAvalonMMHost #(`ADDR_W, `DATA_W));
  NumProxy #(8) depthProxy = ?;
  let {snkReq, srcRsp, ifc} <- toPipelinedAvalonMMHost (depthProxy);

  Reg #(Bit #(`ADDR_W)) addrCnt <- mkReg (0);
  let expectedRsp <- mkSizedFIFOF (8);

  rule genReq (addrCnt < fromInteger ((nb_transaction * (`DATA_W / 8))));
    AvalonMMRequest #(`ADDR_W, `DATA_W) req =
      AvalonMMRequest { address: addrCnt
                      , lock: False
                      , byteenable: ~0
                      , operation: tagged Read };
    tprint ( $format ( "HOST> sent: ", fshow (req)
                     , "\n      expecting: ", fshow (addrCnt) ) );
    snkReq.put (req);
    expectedRsp.enq (addrCnt);
    addrCnt <= addrCnt + (`DATA_W / 8);
  endrule

  rule getRsp (srcRsp.canPeek && expectedRsp.notEmpty);
    tprint ( $format ( "Host> expected rsp: ", fshow (expectedRsp.first)
                     , ", got: ", fshow (srcRsp.peek.operation.Read ) ) );
    if (srcRsp.peek.operation.Read != expectedRsp.first) $finish;
    if (    srcRsp.peek.operation.Read
         >= fromInteger ((nb_transaction - 1) * (`DATA_W / 8)) ) begin
      tprint ($format ("success"));
      $finish;
    end
    srcRsp.drop;
    expectedRsp.deq;
  endrule

  return ifc;
endmodule

module mkPipelinedAvalonMMAgent (PipelinedAvalonMMAgent #(`ADDR_W, `DATA_W));
  NumProxy #(2) depthProxy = ?;
  let {srcReq, snkRsp, ifc} <- toPipelinedAvalonMMAgent (depthProxy);

  // artificial dealy in the agent
  let delay <- mkReg (`DELAY);
  rule dec_delay (delay > 0); delay <= delay - 1; endrule

  rule handleReq (delay == 0);
    let req = srcReq.peek;
    let rsp = AvalonMMResponse { response: 2'h00
                               , operation: tagged Read req.address };
    tprint ( $format ( "AGENT> received: ", fshow (req)
                     , "\n       sending: ", fshow (rsp) ) );
    srcReq.drop;
    snkRsp.put (rsp);
    delay <= `DELAY;
  endrule

  return ifc;
endmodule

module simTop (Empty);
  let h <- mkPipelinedAvalonMMHost;
  let a <- mkPipelinedAvalonMMAgent;
  (* fire_when_enabled, no_implicit_conditions *)
  rule debug;
    $display ("-------------------- <t = %0t> --------------------", $time);
    $display (fshow (h));
    $display (fshow (a));
  endrule
  mkConnection (h, a);

endmodule

`undef ADDR_W
`undef DATA_W
`undef DELAY

endpackage
