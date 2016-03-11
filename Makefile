PROJECT = data_logger
PROJECT_DESCRIPTION = Tellstick data logger
PROJECT_VERSION = 0.1.0

LOCAL_DEPS = sensor_directory data_store

NO_AUTOPATCH += yaws
DEPS = yaws

C_SRC_TYPE = executable
C_SRC_OUTPUT = priv/csensor
LDLIBS = -ltelldus-core

include erlang.mk
