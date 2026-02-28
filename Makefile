# Makefile for building xmloxide as a C library
#
# Usage:
#   make              - build shared and static libraries (release)
#   make shared       - build shared library only (.so / .dylib / .dll)
#   make static       - build static library only (.a / .lib)
#   make debug        - build both in debug mode
#   make example      - build and link the C FFI example
#   make clean        - remove build artifacts

CARGO ?= cargo
CC ?= cc
PROFILE ?= release
CARGO_FLAGS = --features ffi

ifeq ($(PROFILE),release)
  CARGO_FLAGS += --release
  TARGET_DIR = target/release
else
  TARGET_DIR = target/debug
endif

# Detect OS for library naming and link flags
UNAME := $(shell uname -s)
ifeq ($(UNAME),Darwin)
  SHARED_LIB = libxmloxide.dylib
  LDFLAGS = -lpthread -ldl -lm -framework Security
  DYLD = DYLD_LIBRARY_PATH=$(TARGET_DIR)
else ifeq ($(UNAME),Linux)
  SHARED_LIB = libxmloxide.so
  LDFLAGS = -lpthread -ldl -lm
  DYLD = LD_LIBRARY_PATH=$(TARGET_DIR)
else
  SHARED_LIB = xmloxide.dll
  LDFLAGS =
  DYLD =
endif

.PHONY: all shared static debug example clean

shared:
	$(CARGO) rustc --lib $(CARGO_FLAGS) --crate-type=cdylib
	@echo "Built: $(TARGET_DIR)/$(SHARED_LIB)"

static:
	$(CARGO) rustc --lib $(CARGO_FLAGS) --crate-type=staticlib
	@echo "Built: $(TARGET_DIR)/libxmloxide.a"

all: shared static

debug:
	$(MAKE) PROFILE=debug all

example: all
	$(CC) -o ffi_usage examples/ffi_usage.c -Iinclude \
		-L$(TARGET_DIR) -lxmloxide $(LDFLAGS)
	@echo "Built: ./ffi_usage"
	@echo "Run with: $(DYLD) ./ffi_usage"

clean:
	$(CARGO) clean
	rm -f ffi_usage
