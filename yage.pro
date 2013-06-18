
TEMPLATE = app
TARGET = yageexec
CONFIG += qt thread debug
QT = core gui opengl

DESTDIR = .
MOC_DIR = build/moc
OBJECTS_DIR = build/obj

include(src/cpp/core.pri)
