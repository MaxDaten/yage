#include "stdafx.hpp"
#include "Application.hpp"

#include <QtGui/QOpenGLContext>
#include <QtGui/QWindow>

#include <QtCore/QDebug>

#include "GLWindow.hpp"

using namespace yage;

Application::Application(int & argc, char ** argv) : QGuiApplication(argc, argv)
{
	
}


Application::~Application(void)
{
}

int Application::exec(void)
{
    return QGuiApplication::exec();
}

void Application::processEvents( void )
{
	QGuiApplication::processEvents();
}

//-------------------------------------------------------------------------------------------------
// C-Bindings
GRAPHICSCORE_API yage::Application* createApplication( int argc, char** argv )
{
    return new yage::Application(argc, argv);
}

GRAPHICSCORE_API int exec( yage::Application* app )
{
    return app->exec();
}

GRAPHICSCORE_API void processEvents( yage::Application* app )
{
	app->processEvents();
}

GRAPHICSCORE_API void deleteApplication (yage::Application* app)
{
	delete app;
}

