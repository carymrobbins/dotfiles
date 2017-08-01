package ${PACKAGE_NAME}

trait ${NAME}[${GENERIC_ARGS}] {
  def ${METHOD}(${METHOD_ARG_NAME}: ${METHOD_ARG_TYPE}): ${METHOD_RETURN_TYPE}
}

object ${NAME} {

  def apply[${GENERIC_ARGS}](implicit ev: ${NAME}[${GENERIC_ARGS}]): ${NAME}[${GENERIC_ARGS}] = ev

  def instance[${GENERIC_ARGS}](f: ${METHOD_ARG_TYPE} => ${METHOD_RETURN_TYPE}): ${NAME}[${GENERIC_ARGS}] = new ${NAME}[${GENERIC_ARGS}] {
    override def ${METHOD}(${METHOD_ARG_NAME}: ${METHOD_ARG_TYPE}): ${METHOD_RETURN_TYPE} = f(${METHOD_ARG_NAME})
  }
}