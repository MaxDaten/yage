
TEMPLATE = lib
TARGET = yage
QMAKE_CXXFLAGS += -pedantic
CONFIG += qt ##thread debug staticlib
QT = core gui opengl

DESTDIR = .
MOC_DIR = build/moc
OBJECTS_DIR = build/obj

include(src/cpp/core.pri)
