#pragma once

#include "graphics-core.hpp"

#include <QtGui/QGuiApplication>



namespace yage {

class GRAPHICSCORE_API Application : private QGuiApplication
{
    Q_OBJECT
public:
    Application( int & argc, char ** argv );
    virtual ~Application( void );

    int exec( void );

    void processEvents( void );
};

} // namespace yage

//-------------------------------------------------------------------------------------------------
// C-Bindings

extern "C" {
    GRAPHICSCORE_API yage::Application* createApplication (int argc, char** argv);
    GRAPHICSCORE_API void deleteApplication (yage::Application* app);
    GRAPHICSCORE_API int exec(yage::Application* app);
    GRAPHICSCORE_API void processEvents( yage::Application* app );
}
