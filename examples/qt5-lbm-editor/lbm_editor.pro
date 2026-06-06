QT += core widgets

CONFIG += c++17

TARGET   = lbm_editor
TEMPLATE = app

include(../../platform/qt5/lispbm_platform.pri)
include(../../utils/qt5/lispbm_utils_qt.pri)

SOURCES += \
    main.cpp \
    MainWindow.cpp

HEADERS += \
    MainWindow.h
