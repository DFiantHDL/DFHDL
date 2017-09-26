import FIFO::*;

interface Fib;
  method ActionValue#(UInt#(64)) get();
endinterface: Fib

(* synthesize *)
module mkFib(Fib);
  typedef enum Out0, Out1, OutOthers
    StateType deriving (Eq, Bits);

  FIFO#(UInt#(64)) outFifo <- mkFIFO();
  Reg#(UInt#(64)) out_prev <- mkRegU;
  Reg#(UInt#(64)) out_prev2 <- mkRegU;
  Reg#(StateType) state <- mkReg(Out0);

  rule fib;
    UInt#(64) out;
    case (state)
      Out0 :
        out = 0;
        state <= Out1;
      Out1 :
        out = 1;
        state <= OutOthers;
      OutOthers :
        out = out_prev + out_prev2;
    endcase
    outFifo.enq(out);
    out_prev2 <= out_prev;
    out_prev <= out;
  endrule: fib

  method ActionValue#(UInt#(64)) get();
    actionvalue
      outFifo.deq();
      return outFifo.first();
    endactionvalue
  endmethod: get
endmodule: mkFib

