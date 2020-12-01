package DFiant
package lib.ompss

/**
  * Every ompss integration kernel should extend this interface,
  * and add instances of either [[OmpssAXI]] or [[OmpssARR]] to fit the required kernel interface
  */
@df class OmpssIfc extends DFInterface.Unnamed {
  final val ap = new AP_Interface
}
