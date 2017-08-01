package ${PACKAGE_NAME}

trait ${NAME}[${GENERIC_ARGS}] {
  def ${METHOD}(${METHOD_ARG_NAME}: ${METHOD_ARG_TYPE}): ${METHOD_RETURN_TYPE}
}

object ${NAME} {

  def apply[${GENERIC_ARGS}](implicit ev: ${NAME}[${GENERIC_ARGS}]): ${NAME}[${GENERIC_ARGS}] = ev

  def instance[${GENERIC_ARGS}](f: ${METHOD_ARG_TYPE} => ${METHOD_RETURN_TYPE}): ${NAME}[${GENERIC_ARGS}] = new ${NAME}[${GENERIC_ARGS}] {
    override def ${METHOD}(${METHOD_ARG_NAME}: ${METHOD_ARG_TYPE}): ${METHOD_RETURN_TYPE} = f(${METHOD_ARG_NAME})
  }

  final class Ops[${GENERIC_ARGS}](val repr: ${METHOD_ARG_TYPE}) extends AnyVal {
    def ${METHOD}(implicit ev: ${NAME}[${GENERIC_ARGS}]): ${METHOD_RETURN_TYPE} = ev.${METHOD}(repr)
  }

  trait ToOps {
    implicit def to${NAME}Ops[${GENERIC_ARGS}](x: ${METHOD_ARG_TYPE}): Ops[${GENERIC_ARGS}] = new Ops(x)
  }
}