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

package AvalonMemoryMapped;

import FIFOF :: *;
import SpecialFIFOs :: *;
import Assert :: *;
import Connectable :: *;

import BlueBasics :: *;

// Flit types
////////////////////////////////////////////////////////////////////////////////

///////////////////
// Host -> Agent //
///////////////////

typedef struct {
  Bit #(t_byte_addr_w) address;
  Bool lock;
  // Bit #(t_burstcount_w) burstcount;
  union tagged {
    void Read;
    struct {
      Bit #(TDiv #(t_data_w, 8)) byteenable;
      Bit #(t_data_w) writedata;
    } Write;
  } operation;
} AvalonMMRequest #( numeric type t_byte_addr_w
                   , numeric type t_data_w )
deriving (Bits);

typedef struct {
  Bit #(t_byte_addr_w) address;
  Bool lock;
  // Bit #(t_burstcount_w) burstcount;
  Bool read;
  Bool write;
  Bit #(TDiv #(t_data_w, 8)) byteenable;
  Bit #(t_data_w) writedata;
} AvalonMMHost2Agent #( numeric type t_byte_addr_w
                      , numeric type t_data_w )
deriving (Bits);

function AvalonMMHost2Agent #(addr_, data_)
  avalonMMReq2Host2Agent (AvalonMMRequest #(addr_, data_) req) =
  AvalonMMHost2Agent {
    address: req.address
  , lock: req.lock
  , read: req.operation matches tagged Read ? True : False
  , write: req.operation matches tagged Write .* ? True : False
  , byteenable: req.operation.Write.byteenable
  , writedata: req.operation.Write.writedata
  };

///////////////////
// Agent -> Host //
///////////////////

typedef struct {
  Bit #(2) response;
  union tagged {
    Bit #(t_data_w) Read;
    void Write;
  } operation;
} AvalonMMResponse #(numeric type t_data_w)
deriving (Bits);

typedef struct {
  Bit #(2) response;
  Bit #(t_data_w) readdata;
  Bool waitrequest;
  Bool readdatavalid;
  Bool writeresponsevalid;
} AvalonMMAgent2Host #(numeric type t_data_w)
deriving (Bits);

function AvalonMMResponse #(data_)
  avalonMMAgent2Host2Rsp (AvalonMMAgent2Host #(data_) rsp) =
  AvalonMMResponse { response: rsp.response
                   , operation: rsp.readdatavalid
                              ? Read (rsp.readdata)
                              : rsp.writeresponsevalid
                              ? Write
                              : ? };

// Interfaces
////////////////////////////////////////////////////////////////////////////////

////////////
// Simple //
////////////

(* always_ready, always_enabled *)
interface AvalonMMHost #( numeric type t_byte_addr_w
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
interface AvalonMMAgent #( numeric type t_byte_addr_w
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

///////////////
// Pipelined //
///////////////

(* always_ready, always_enabled *)
interface PipelinedAvalonMMHost #( numeric type t_byte_addr_w
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
                                           , Bit #(t_data_w) readdata
                                           , Bool readdatavalid
                                           , Bool writeresponsevalid );
endinterface

(* always_ready, always_enabled *)
interface PipelinedAvalonMMAgent #( numeric type t_byte_addr_w
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
  method Bool readdatavalid;
  method Bool writeresponsevalid;
endinterface

// "transactors"
////////////////////////////////////////////////////////////////////////////////

// Simple

typedef enum {Idle, Wait} ToAvalonMMHostState deriving (Bits, Eq);
module toAvalonMMHost
  ( Tuple3 #( Sink #(AvalonMMRequest #(t_byte_addr_w, t_data_w))
            , Source #(AvalonMMResponse #(t_data_w))
            , AvalonMMHost #(t_byte_addr_w, t_data_w) ) );

  // responses / agent to host signaling
  FIFOF #(AvalonMMResponse #(t_data_w)) ff_a2h <- mkFIFOF;
  Wire #(AvalonMMAgent2Host #(t_data_w)) w_a2h <- mkBypassWire;
  // requests / host to agent signaling
  let ff_h2a <- mkBypassFIFOF;
  let src_h2a = mapSource (avalonMMReq2Host2Agent, toSource (ff_h2a));
  let w_h2a <- mkDWire (AvalonMMHost2Agent { address: ?
                                           , lock: ?
                                           // burstcount
                                           , read: False
                                           , write: False
                                           , byteenable: ?
                                           , writedata: ?
                                           });
  // state register
  Reg #(ToAvalonMMHostState) r_state <- mkReg (Idle);

  //////////////////////////////////////////////////////////////////////////////

  Bool can_sample_request = r_state == Idle && src_h2a.canPeek;

  rule sample_request (r_state == Idle && src_h2a.canPeek);
    w_h2a <= src_h2a.peek;
  endrule

  rule consume_request (r_state == Idle && src_h2a.canPeek
                                        && !w_a2h.waitrequest);
    src_h2a.drop;
    r_state <= Wait;
  endrule

  rule forward_response (r_state == Wait && ff_a2h.notFull
                                         && !w_a2h.waitrequest);
    ff_a2h.enq (avalonMMAgent2Host2Rsp (w_a2h));
    r_state <= Idle;
  endrule

  //////////////////////////////////////////////////////////////////////////////

  // AvalonMMHost interface
  let avmmh = interface AvalonMMHost;
    method Bit #(t_byte_addr_w) address = w_h2a.address;
    method Bool read = w_h2a.read;
    method Bool write = w_h2a.write;
    method Bit #(TDiv #(t_data_w, 8)) byteenable = w_h2a.byteenable;
    method Bit #(t_data_w) writedata = w_h2a.writedata;
    method Bool lock = w_h2a.lock;
    method Action agent2host ( Bool waitrequest
                             , Bit #(2) response
                             , Bit #(t_data_w) readdata ) = action
      w_a2h <= AvalonMMAgent2Host { waitrequest: waitrequest
                                  , response: response
                                  , readdata: readdata
                                  , readdatavalid: ?
                                  , writeresponsevalid: ? };
    endaction;
  endinterface;

  return tuple3 (toSink (ff_h2a), toSource (ff_a2h), avmmh);

endmodule

// Pipelined

module toPipelinedAvalonMMHost #(Integer max_depth)
  ( Tuple3 #( Sink #(AvalonMMRequest #(t_byte_addr_w, t_data_w))
            , Source #(AvalonMMResponse #(t_data_w))
            , PipelinedAvalonMMHost #(t_byte_addr_w, t_data_w) ) );

  // responses / agent to host signaling
  Wire #(AvalonMMAgent2Host #(t_data_w)) w_a2h <- mkBypassWire;
  FIFOF #(AvalonMMResponse #(t_data_w)) ff_a2h <- mkSizedFIFOF (max_depth);
  // requests / host to agent signaling
  let ff_h2a <- mkSizedFIFOF (max_depth);
  let src_h2a = mapSource (avalonMMReq2Host2Agent, toSource (ff_h2a));
  let w_h2a <- mkDWire (AvalonMMHost2Agent { address: ?
                                           , lock: ?
                                           // burstcount
                                           , read: False
                                           , write: False
                                           , byteenable: ?
                                           , writedata: ?
                                           });

  //////////////////////////////////////////////////////////////////////////////

  // can sample a request only if (1) there is one pending
  //                              (2) there is space for a response
  Bool can_sample_request = src_h2a.canPeek && ff_a2h.notFull;

  rule sample_request (can_sample_request); w_h2a <= src_h2a.peek; endrule

  rule consume_request (can_sample_request && !w_a2h.waitrequest);
    src_h2a.drop;
  endrule

  rule forward_response
    ((w_a2h.readdatavalid || w_a2h.writeresponsevalid) && ff_a2h.notFull);
    ff_a2h.enq (avalonMMAgent2Host2Rsp (w_a2h));
  endrule

  //////////////////////////////////////////////////////////////////////////////

  // PipelinedAvalonMMHost interface
  let avmmh = interface PipelinedAvalonMMHost;
    method Bit #(t_byte_addr_w) address = w_h2a.address;
    method Bool read = w_h2a.read;
    method Bool write = w_h2a.write;
    method Bit #(TDiv #(t_data_w, 8)) byteenable = w_h2a.byteenable;
    method Bit #(t_data_w) writedata = w_h2a.writedata;
    method Bool lock = w_h2a.lock;
    method Action agent2host ( Bool waitrequest
                             , Bit #(2) response
                             , Bit #(t_data_w) readdata
                             , Bool readdatavalid
                             , Bool writeresponsevalid ) = action
      w_a2h <= AvalonMMAgent2Host { waitrequest: waitrequest
                                  , response: response
                                  , readdata: readdata
                                  , readdatavalid: readdatavalid
                                  , writeresponsevalid: writeresponsevalid };
    endaction;
  endinterface;

  return tuple3 (toSink (ff_h2a), toSource (ff_a2h), avmmh);

endmodule

endpackage
