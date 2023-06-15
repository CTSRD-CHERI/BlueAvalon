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

import FIFO :: *;
import FIFOF :: *;
import FIFOLevel::*;
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
            , PipelinedAvalonMMHost #(t_byte_addr_w, t_data_w) ) )
  provisos (Add #(_a, TLog #(TDiv #(t_data_w, 8)), t_byte_addr_w));

   // bypass wires for incoming Avalon master signals
   // N.B. avalon master address is a byte address, so need to add 2 bits
   Reg#(Bit#(t_byte_addr_w))  address_r       <- mkReg(0);
   Reg#(Bool)  lock_r       <- mkReg(False);
   Reg#(Bit #(TDiv #(t_data_w, 8)))  byteenable_r     <- mkReg(0);
   Reg#(Bit#(t_data_w))  writedata_r     <- mkReg(0);
   Reg#(Bool)         read_r          <- mkReg(False);
   Reg#(Bool)         write_r         <- mkReg(False);
   PulseWire          signal_read     <- mkPulseWire;
   PulseWire          signal_write    <- mkPulseWire;
   Wire#(Bool)        avalonwait      <- mkBypassWire;
   Wire#(Bool)        avalonreadvalid <- mkBypassWire;
   Wire#(Bit#(2)) avalonresponse  <- mkBypassWire;
   Wire#(Bit#(t_data_w)) avalonreaddata  <- mkBypassWire;
   
   // buffer data returned
   // TODO: could this buffer be removed by not initiating the transaction
   // until the returndata get operation was active, then do the memory 
   // transaction and return the value to the get without buffering?
   //  - possibly not if the interface is fully pipelined because there
   //    can be several transactions ongoing (several addresses issued, etc.)
   //    before data comes back
   
   // FIFO of length 4 which is:
   // Unguarded enq since it it guarded by the bus transaction initiation
   // Guarded deq
   // Unguarded count so isLessThan will not block
   FIFOLevelIfc#(AvalonMMResponse#(t_data_w),4) datareturnbuf <- mkGFIFOLevel(True,False,True);
   FIFO#(Bool) pending_acks <- mkSizedFIFO(4);
   FIFO#(Bit #(0)) pending_write_acks <- mkSizedFIFO(4);
   
   let write_ack = write_r && !read_r && !avalonwait;
   
   rule buffer_data_read (avalonreadvalid && (pending_acks.first));
      datareturnbuf.enq(AvalonMMResponse { response: avalonresponse
                                         , operation: tagged Read avalonreaddata } );
      $display("   %05t: Avalon2ClientServer returning data",$time);
      pending_acks.deq;
   endrule
   
   rule data_read_error (avalonreadvalid && (!pending_acks.first));
      $display("ERROR: Server2AvalonPipelinedMaster - read returned when expeting a write ack");
   endrule
   
   rule buffer_data_write_during_readvalid (avalonreadvalid && write_ack);
      pending_write_acks.enq(?);
   endrule
   
   rule signal_data_write (!avalonreadvalid && write_ack && (!pending_acks.first));
      datareturnbuf.enq(AvalonMMResponse { response: avalonresponse
                                         , operation: tagged Write } );
      pending_acks.deq;
   endrule

   rule resolve_pending_write_acks (!avalonreadvalid && !write_ack && (!pending_acks.first));
      pending_write_acks.deq; // N.B. only fires if this dequeue can happen
      datareturnbuf.enq(AvalonMMResponse { response: avalonresponse
                                         , operation: tagged Write } );
      pending_acks.deq;
   endrule
   
   (* no_implicit_conditions *)
   rule do_read_reg;
      if(signal_read) read_r <= True;
      else if(!avalonwait) read_r <= False;
   endrule
   
   (* no_implicit_conditions *)
   rule do_write_reg;
      if(signal_write) write_r <= True;
      else if(!avalonwait) write_r <= False;
   endrule
   
  //////////////////////////////////////////////////////////////////////////////

  // PipelinedAvalonMMHost interface
  Bit #(TLog #(TDiv #(t_data_w, 8))) zeroes = 0;
  let avmmh = interface PipelinedAvalonMMHost;
    method Bit #(t_byte_addr_w) address = {truncateLSB (address_r), zeroes};
    method Bool read = read_r;
    method Bool write = write_r;
    method Bit #(TDiv #(t_data_w, 8)) byteenable = byteenable_r;
    method Bit #(t_data_w) writedata = writedata_r;
    method Bool lock = lock_r;
    method Action agent2host ( Bool waitrequest
                             , Bit #(2) response
                             , Bit #(t_data_w) readdata
                             , Bool readdatavalid
                             , Bool writeresponsevalid ) = action
      avalonresponse <= response;
      avalonreaddata <= readdata;
      avalonreadvalid <= readdatavalid;
      avalonwait <= waitrequest;
    endaction;
  endinterface;

  let reqSnk = interface Sink;
    method canPut = !avalonwait && datareturnbuf.isLessThan(2) && impCondOf (pending_acks.enq);
    method Action put (AvalonMMRequest #(t_byte_addr_w, t_data_w) req) if (!avalonwait && datareturnbuf.isLessThan(2));
      address_r     <= req.address;
      lock_r     <= req.lock;
      byteenable_r  <= req.operation.Write.byteenable;
      writedata_r   <= req.operation.Write.writedata;
      pending_acks.enq (req.operation matches tagged Read ? True : False);
      case (req.operation) matches
        tagged Read:  signal_read.send;
        tagged Write .*: signal_write.send;
      endcase
    endmethod
  endinterface;

  return tuple3 (reqSnk, toGuardedSource (datareturnbuf), avmmh);

endmodule

//typedef struct {
//  Bit #(2) response;
//  union tagged {
//    Bit #(t_data_w) Read;
//    void Write;
//  } operation;
//} AvalonMMResponse #(numeric type t_data_w)
//deriving (Bits);

endpackage
