PROJECT = data_logger
PROJECT_DESCRIPTION = Tellstick data logger
PROJECT_VERSION = 0.2.0

LOCAL_DEPS = sensor_directory data_store device_directory

DEPS = cowboy
deb_cowboy_commit = master

C_SRC_TYPE = executable
C_SRC_OUTPUT = priv/csensor
LDLIBS = -ltelldus-core

include erlang.mk
