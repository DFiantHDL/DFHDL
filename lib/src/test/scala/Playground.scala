import dfhdl.*
import dfhdl.hw.constraints.timing.clock

@clock(edge = _.rising)
@top(false) class Foo extends RTDesign
