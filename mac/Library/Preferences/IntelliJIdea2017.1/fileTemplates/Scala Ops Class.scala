#if (${PACKAGE_NAME})
package ${PACKAGE_NAME}
#end
#if (${GENERIC_ARGS})
final class ${NAME}[${GENERIC_ARGS}](val repr: ${REPR_CLASS}) extends AnyVal {

}

trait To${NAME} {
  implicit def to${NAME}[${GENERIC_ARGS}](x: ${REPR_CLASS}): ${NAME}[${GENERIC_ARGS}] = new ${NAME}[${GENERIC_ARGS}](x)
}
#else
final class ${NAME}(val repr: ${REPR_CLASS}) extends AnyVal {

}

trait To${NAME} {
  implicit def to${NAME}(x: ${REPR_CLASS}): ${NAME} = new ${NAME}(x)
}
#end