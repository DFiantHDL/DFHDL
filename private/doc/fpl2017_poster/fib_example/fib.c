unsigned long fib() {
  typedef enum {Out0, Out1, OutOthers} state_type;
  static state_type state = Out0;
  static unsigned long out_prev;
  static unsigned long out_prev2;
  unsigned long out = 0;
  switch (state) {
    case Out0 :
      state = Out1;
      out = 0;
      break;
    case Out1 :
      state = OutOthers;
      out = 1;
      break;
    case OutOthers :
      out = out_prev + out_prev2;
  }
  out_prev2 = out_prev;
  out_prev = out;
  return out;
}