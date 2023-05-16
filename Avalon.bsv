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

package Avalon;

import Assert :: *;
import BlueBasics :: *;

// Flit types
////////////////////////////////////////////////////////////////////////////////

typedef struct {
  Bit #(t_byte_addr_w) address;
  Bool lock;
  union tagged {
    void Read;
    struct {
      Bit #(TDiv #(t_data_w, 8)) byteenable;
      Bit #(t_data_w) writedata;
    } Write;
  } operation;
} AvalonRequest #( numeric type t_byte_addr_w
                 , numeric type t_data_w )
deriving (Bits);

typedef AvalonRequest #(t_byte_addr_w, t_data_w)
        AvalonHostRequest #(numeric type t_byte_addr_w, numeric type t_data_w);

typedef AvalonRequest #( TAdd #(t_word_addr_w, TLog #(TDiv #(t_data_w, 8)))
                       , t_data_w)
        AvalonAgentRequest #(numeric type t_word_addr_w, numeric type t_data_w);

function AvalonAgentRequest#(t_word_addr_w, t_data_w)
  avalonHostToAgentRequest (AvalonHostRequest #(t_byte_addr_w, t_data_w) r)
  provisos ( NumAlias #(t_offset_bits_w, TLog #( TDiv #(t_data_w, 8)))
           , Add #(t_word_addr_w, t_offset_bits_w, t_byte_addr_w)
           , Add #( TAdd #(t_word_addr_w, TLog#(TDiv#(t_data_w, 8)))
                  , _a
                  , t_byte_addr_w ) ) =
  AvalonAgentRequest { address: truncateLSB (r.address)
                     , lock: r.lock
                     , operation: r.operation };

typedef struct {
  Bit #(2) response;
  union tagged {
    Bit #(t_data_w) Read;
    void Write;
  } operation;
} AvalonResponse #(numeric type t_data_w)
deriving (Bits);

// Interfaces
////////////////////////////////////////////////////////////////////////////////

(* always_ready, always_enabled *)
interface AvalonHost #( numeric type t_byte_addr_w
                      , numeric type t_data_w );
  // host to agent
  method Bit #(t_byte_addr_w) address;
  method Bool read;
  method Bool write;
  method Bit #(TDiv #(t_data_w, 8)) byteenable;
  method Bit #(t_data_w) writedata;
  method Bool lock;
  // agent to host
  (* prefix="" *) method Action agent2host ( Bool waitrequest
                                           , Bit #(2) response
                                           , Bit #(t_data_w) readdata );
endinterface

(* always_ready, always_enabled *)
interface AvalonAgent #( numeric type t_byte_addr_w
                       , numeric type t_data_w );
  // host to agent
  (* prefix="" *) method Action host2agent (
      Bit #(t_byte_addr_w) address
    , Bool read
    , Bool write
    , Bit #(TDiv #(t_data_w, 8)) byteenable
    , Bit #(t_data_w) writedata
    , Bool lock );
  // agent to host
  method Bool waitrequest;
  method Bit #(2) response;
  method Bit #(t_data_w) readdata;
endinterface

// "transactors"
////////////////////////////////////////////////////////////////////////////////

typedef enum {Idle, Wait} ToAvalonHostState deriving (Bits, Eq);
module toAvalonHost #(
  Source #(AvalonHostRequest #(t_byte_addr_w, t_data_w)) reqSrc
, Sink #(AvalonResponse #(t_data_w)) rspSnk
) (AvalonHost #(t_byte_addr_w, t_data_w));

  // agent to host signaling
  Wire #(Bool) w_waitrequest <- mkBypassWire;
  Wire #(Bit #(2)) w_response <- mkBypassWire;
  Wire #(Bit #(t_data_w)) w_readdata <- mkBypassWire;
  // host to agent signaling
  Wire #(Bit #(t_byte_addr_w)) r_address[2] <- mkCRegU (2);
  Wire #(Bool) r_read[2] <- mkCReg (2, False);
  Wire #(Bool) r_write[2] <- mkCReg (2, False);
  Wire #(Bit #(TDiv #(t_data_w, 8))) r_byteenable[2] <- mkCRegU (2);
  Wire #(Bit #(t_data_w)) r_writedata[2] <- mkCRegU (2);
  Wire #(Bool) r_lock[2] <- mkCRegU (2);
  // state register
  Reg #(ToAvalonHostState) r_state <- mkReg (Idle);

  //////////////////////////////////////////////////////////////////////////////

  // when in idle state, if a request is available, latch it on the interface
  // and transition to wait state
  rule idle_state (r_state == Idle && reqSrc.canPeek);
    let avReq <- get (reqSrc);
    r_address[0] <= avReq.address;
    r_lock[0] <= avReq.lock;
    case (avReq.operation) matches
      tagged Read: r_read[0] <= True;
      tagged Write {byteenable: .be, writedata: .wd}: begin
        r_write[0] <= True;
        r_byteenable[0] <= be;
        r_writedata[0] <= wd;
      end
    endcase
    r_state <= Wait;
  endrule

  // when in wait state, if a response is available (!w_waitrequest) and can be
  // forwarded, forward it, reset the read/write state and transition back to
  // idle state
  continuousAssert ( !(r_read[0] && r_write[0])
                   , "Can't be waiting for read and write" );
  rule wait_state (r_state == Wait && rspSnk.canPut && !w_waitrequest);
    let rsp = AvalonResponse { response: w_response
                             , operation: ? };
    if (r_read[0])  rsp.operation = Read (w_readdata);
    if (r_write[0]) rsp.operation = Write;
    rspSnk.put (rsp);
    r_read[1] <= False;
    r_write[1] <= False;
    r_state <= Idle;
  endrule

  //////////////////////////////////////////////////////////////////////////////

  // interface
  method Bit #(t_byte_addr_w) address = r_address[1];
  method Bool read = r_read[1];
  method Bool write = r_write[1];
  method Bit #(TDiv #(t_data_w, 8)) byteenable = r_byteenable[1];
  method Bit #(t_data_w) writedata = r_writedata[1];
  method Bool lock = r_lock[1];
  method Action agent2host ( Bool waitrequest
                           , Bit #(2) response
                           , Bit #(t_data_w) readdata ) = action
    w_waitrequest <= waitrequest;
    w_response <= response;
    w_readdata <= readdata;
  endaction;

endmodule

endpackage
