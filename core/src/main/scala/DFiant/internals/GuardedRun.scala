/*
 *     This file is part of DFiant.
 *
 *     DFiant is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU Lesser General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     any later version.
 *
 *     DFiant is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU Lesser General Public License for more details.
 *
 *     You should have received a copy of the GNU Lesser General Public License
 *     along with DFiant.  If not, see <https://www.gnu.org/licenses/>.
 */

package DFiant.internals

class GuardedRun(block : => Unit) {
  import GuardedRun.RunStatus
  private var runStatus : RunStatus = RunStatus.Idle
  def run() : Unit = runStatus match {
    case RunStatus.Idle =>
      runStatus = RunStatus.Running
      block
    case RunStatus.Done =>
      runStatus = RunStatus.Running

  }
}
object GuardedRun {
  sealed trait RunStatus
  object RunStatus {
    case object Idle extends RunStatus
    case object Done extends RunStatus
    case object Running extends RunStatus
    case object Restart extends RunStatus
  }
}
