# Hello Hardware World {#hello-world}

## The Basic DFHDL Program

Since DFHDL is a Scala library, we are creating a Scala program that takes DFHDL designs and compiles (transpiles) them into lower representations (e.g., VHDL or Verilog). As such, some of DFHDL's compilation process is done statically via the Scala compiler and the rest during the Scala runtime execution. The Scala code below describes a program that runs the DFHDL compiler on an 8-bit overlapping counter design, `Counter8`. 

```{.scala .copy}
--8<-- "docs/getting-started/hello-world/scala-project/Counter8.scala"
```

/// admonition | Writing a DFHDL compilation program – as easy as 01-10-11!
    type: summary
1. `#!scala import dfhdl.*` once per source file, to import all the required namespace objects, types, and functionality.
2. `#!scala class _design_name_ extends RTDesign:` to define your register-transfer (RT) domain design. Populate your design with the required interface and functionality. DFHDL supports two additional design domains: dataflow (DF), and event-driven (ED).
3. Add `#!scala @top` annotation to your top-level design (e.g., `#!scala @top class top_design_name_ ...`) to automatically create a compilation program entry point for the design, instantiate it, elaborate it, compile it to Verilog or VHDL (see compiler options), and finally commit the files to disk.
///

## Run It In Your Browser

/// details | Run it here
    type: dfhdl
```scastie
--8<-- "docs/getting-started/hello-world/scala-project/Counter8.scala"
```
///

For more examples that are available to run in your browser, see the [relevant section][run-in-browser].

## Run It On Your System

To run this example on your system, make sure to first follow the [initial setup][getting-started] instructions.

You have several options to run Scala programs on your system:

* For this simple `Counter8` example, you can just use the simplest [scala-single-file][scala-single-file] approach. 
* For common DFHDL projects, we recommend using the [scala project][scala-project] approach. 
* For complex, full-production DFHDL projects, you may need to use an [sbt project][sbt-project], but this is usually not required.

### Scala Single File

/// details | View the scala single file example
    type: dfhdl
```{.scala .copy title="Counter8.scala"}
--8<-- "docs/getting-started/hello-world/scala-single-file/Counter8.scala"
```
///

```{.console .copy linenums="0" title="Download and run in your terminal"}
curl -o Counter8.scala https://dfianthdl.github.io/getting-started/hello-world/scala-single-file/Counter8.scala
scala run ./Counter8.scala
```

For more information, please run `scala run --help` or consult the [online documentation](https://scala-cli.virtuslab.org/docs/commands/run){target="_blank"}.

### Scala Project

/// details | View the scala project files example
    type: dfhdl
```{.scala .copy title="projectFolder/project.scala"}
--8<-- "docs/getting-started/hello-world/scala-project/project.scala"
```

```{.scala .copy title="projectFolder/Counter8.scala"}
--8<-- "docs/getting-started/hello-world/scala-project/Counter8.scala"
```
///

```{.console .copy linenums="0" title="Download and run in your terminal"}
curl -o project.scala https://dfianthdl.github.io/getting-started/hello-world/scala-project/project.scala
curl -o Counter8.scala https://dfianthdl.github.io/getting-started/hello-world/scala-project/Counter8.scala
scala run .
```

For more information, please run `scala run --help` or consult the [online documentation](https://scala-cli.virtuslab.org/docs/commands/run){target="_blank"}.

### sbt Project

The best way to get started with a DFHDL sbt project is to clone our template from GitHub:

```{.console .copy linenums="0" title="Clone and run in your terminal"}
git clone https://github.com/DFiantHDL/dfhdl-template
cd dfhdl-template
sbt run
```

For more information, please consult the [sbt documentation](https://www.scala-sbt.org/1.x/docs/){target="_blank"}.

### Recommended Scala Formatting for DFHDL

We recommend actively using [Scalafmt](https://scalameta.org/scalafmt/){target="_blank"}, a code formatter for Scala that integrates well with your toolchain. The following setting is recommended for DFHDL designs:

/// details | View the Scalafmt recommended configuration file
    type: dfhdl
```{.toml .copy title="projectFolder/.scalafmt.conf"}
--8<-- "docs/getting-started/hello-world/scala-project/.scalafmt.conf"
```
///

```{.console .copy linenums="0" title="Download it via your terminal"}
curl -o .scalafmt.conf https://dfianthdl.github.io/getting-started/hello-world/scala-project/.scalafmt.conf
```

For more information, please consult the [Scalafmt documentation](https://scalameta.org/scalafmt/docs/configuration.html){target="_blank"}.
