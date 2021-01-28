The Scala code in Fig. 1b describes a program that runs the DFiant compiler on an identity function dataflow design, `ID`. Since DFiant is a Scala library some if its compilation process is done statically via the Scala compiler and the rest during the Scala runtime execution. 

!!! summary "Writing a DFiant compilation program – easy as 1-2-3!"

	1. `#!scala import DFiant._` to import all the required namespace fields
	2. `#!scala trait _design_name_ extends DFDesign {}` to define your dataflow design. Populate your design with the required dataflow functionality.
	3. `#!scala object _program_name_ extends DFApp.VHDLCompiler[_design_name_]` to create your compilation program entry point.

