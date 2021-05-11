package edu.colorado.csci3155.project1


sealed trait StackMachineInstruction
case class LoadIns(s: String) extends StackMachineInstruction
case class  StoreIns(s: String) extends StackMachineInstruction
case class PushIns(f: Double) extends StackMachineInstruction
case object AddIns extends StackMachineInstruction
case object SubIns extends StackMachineInstruction
case object MultIns extends StackMachineInstruction
case object DivIns extends StackMachineInstruction
case object ExpIns extends StackMachineInstruction
case object LogIns extends StackMachineInstruction
case object SinIns extends StackMachineInstruction
case object CosIns extends StackMachineInstruction
case object PopIns extends StackMachineInstruction


object StackMachineEmulator {



    /* Function emulateSingleInstruction
        Given a list of doubles to represent a stack
              a map from string to double precision numbers for the environment
        and   a single instruction of type StackMachineInstruction
        Return a tuple that contains the
              modified stack that results when the instruction is executed.
              modified environment that results when the instruction is executed.

        Make sure you handle the error cases: eg., stack size must be appropriate for the instruction
        being executed. Division by zero, log of a non negative number
        Throw an exception or assertion violation when error happens.
     */
    def emulateSingleInstruction(stack: List[Double],
                                 env: Environment.t,
                                 ins: StackMachineInstruction): (List[Double], Environment.t) = {
        // TODO: Implement this
        ins match {
            case LoadIns(ident) => {
                stack match {
                    case Nil => throw new IllegalArgumentException(s"Stack cannot be empty when executing LoadIns")
                    case head::tail => (tail, Environment.extend(ident, head, env))
                    case _ => throw new IllegalArgumentException(s"Something went wrong with LoadIns")
                }
            }
            case StoreIns(ident) => {
                val v = Environment.lookup(ident, env)
                (v::stack, env)
            }
            case PushIns(d) => (d::stack, env)
            case PopIns => {
                stack match {
                    case Nil => throw new IllegalArgumentException(s"Stack cannot be empty when executing PopIns")
                    case head::tail => (tail, env)
                    case _ => throw new IllegalArgumentException(s"Something went wrong with PopIns")
                }
            }
            case AddIns => {
                stack match {
                    case v1::(v2::tail) => {
                        val sum = v1 + v2
                        (sum::tail, env)
                    }
                    case v1::Nil => throw new IllegalArgumentException(s"Stack needs two elements to perform AddIns")
                    case Nil => throw new IllegalArgumentException(s"Stack cannot be empty when perfoming AddIns")
                    case _ => throw new IllegalArgumentException(s"Something went wrong with AddIns")
                }
            }
            case SubIns => {
                stack match {
                    case v1::(v2::tail) => {
                        val diff = v2 - v1
                        (diff::tail, env)
                    }
                    case v1::Nil => throw new IllegalArgumentException(s"Stack needs two elements to perform SubIns")
                    case Nil => throw new IllegalArgumentException(s"Stack cannot be empty when perfoming SubIns")
                    case _ => throw new IllegalArgumentException(s"Something went wrong with SubIns")
                }
            }
            case MultIns => {
                stack match {
                    case v1::(v2::tail) => {
                        val product = v2*v1
                        (product::tail, env)
                    }
                    case v1::Nil => throw new IllegalArgumentException(s"Stack needs two elements to perform MultIns")
                    case Nil => throw new IllegalArgumentException(s"Stack cannot be empty when perfoming MultIns")
                    case _ => throw new IllegalArgumentException(s"Something went wrong with MultIns")
                }
            }
            case DivIns => {
                stack match {
                    case 0.0::tail => throw new IllegalArgumentException(s"Divide by zero error")
                    case v1::(v2::tail) => {
                        val dividend = v2/v1
                        (dividend::tail, env)
                    }
                    case v1::Nil => throw new IllegalArgumentException(s"Stack needs two elements to perform DivIns")
                    case Nil => throw new IllegalArgumentException(s"Stack cannot be empty when performing DivIns")
                    case _ => throw new IllegalArgumentException(s"Something went wrong with DivIns")
                }
            }
            case LogIns => {
                stack match {
                    case v::tail if v <= 0.0 => throw new IllegalArgumentException(s"Cannot take log of negative number")
                    case Nil => throw new IllegalArgumentException(s"Empty stack when performing LogIns")
                    case v::tail if v > 0.0 => {
                        val result = math.log(v)
                        (result :: tail, env)
                    }
                    case _ =>  throw new IllegalArgumentException(s"Something went wrong with LogIns")
                }
            }
            case ExpIns => {
                stack match {
                    case Nil => throw new IllegalArgumentException(s"Empty stack when performing ExpIns")
                    case v::tail => {
                        val result = math.exp(v)
                        (result :: tail, env)
                    }
                    case _ => throw new IllegalArgumentException(s"Something went wrong with LogIns")
                }
            }
            case SinIns => {
                stack match {
                    case Nil => throw new IllegalArgumentException(s"Empty stack when performing SinIns")
                    case v::tail => {
                        val result = math.sin(v)
                        (result::tail, env)
                    }
                    case _ => throw new IllegalArgumentException(s"Something went wrong with SinIns")
                }
            }
            case CosIns => {
                stack match {
                    case Nil => throw new IllegalArgumentException(s"Empty stack when performing CosIns")
                    case v::tail => {
                        val result = math.cos(v)
                        (result::tail, env)
                    }
                    case _ => throw new IllegalArgumentException(s"Something went wrong with CosIns")
                }
            }
        }
    }

    /* Function emulateStackMachine
       Execute the list of instructions provided as inputs using the
       emulateSingleInstruction function.
       Use foldLeft over list of instruction rather than a for loop if you can.
       Return value must be a double that is the top of the stack after all instructions
       are executed.
     */
    def emulateStackMachine(instructionList: List[StackMachineInstruction]): Environment.t = {
        // TODO: Implement this
        val stack: List[Double] = Nil
        val env: Environment.t = Nil

        val (newStack, newEnv) = instructionList.foldLeft (stack, env) { case ((stack, env), ins) =>
            emulateSingleInstruction(stack, env, ins)
        }
        newEnv
    }
}