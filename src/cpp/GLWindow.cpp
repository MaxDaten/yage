#include "stdafx.hpp"
#include "GLWindow.hpp"

#include <QtGui/QOpenGLContext>
#include <QtCore/QList>
#include <QtCore/QByteArray>
#include <QtCore/QDebug>

#include <algorithm>
#include <math.h>

yage::GLWindow::GLWindow(QScreen *screen) 
    : QWindow(screen)
    , m_context(0)
{
    // Tell Qt we will use OpenGL for this window
    setSurfaceType( OpenGLSurface );

    QSurfaceFormat format;
    format.setDepthBufferSize( 24 );
    format.setMajorVersion( 4 );
    format.setMinorVersion( 3 );
    format.setSamples( 4 );
    format.setProfile( QSurfaceFormat::CoreProfile );
    setFormat( format );
}


yage::GLWindow::~GLWindow(void)
{
    qDebug() << "destr window";
    delete m_context;
}

void yage::GLWindow::resize( int w, int h )
{
    QWindow::resize(w, h);
}

int yage::GLWindow::width( void )
{
    return QWindow::width();
}

int yage::GLWindow::height( void )
{
    return QWindow::height();
}

double yage::GLWindow::pixelRatio ( void )
{
    return QWindow::devicePixelRatio();
}


void yage::GLWindow::show( void )
{
    QWindow::show();
}

void yage::GLWindow::beginDraw ( void )
{
    if (!isExposed()) {
        return;
    }

    bool needInit = false;
    if (!m_context) {
        qDebug() << "start ctx";
        // Specify the format and create platform-specific surface
        
        // Create an OpenGL context
        m_context = new QOpenGLContext( this );
        m_context->setFormat( requestedFormat() );
        m_context->create();

        if (!m_context->isValid())
            qCritical() << "invalid context";

        // Make the context current on this window

        QList<QByteArray> extensions = m_context->extensions().toList();
        qDebug() << "Version: " << m_context->format().majorVersion() << "." << m_context->format().minorVersion();
        qDebug() << "Profile: " << m_context->format().profile();
        //std::sort(extensions.first(), extensions.last());
        qDebug() << "Supported extensions (" << extensions.count() << ")";
        foreach ( const QByteArray &extension, extensions )
            qDebug() << "    " << extension;

        needInit = true;
    }

    m_context->makeCurrent( this );


    if (needInit) {
        initializeOpenGLFunctions();

        QString versionString(QLatin1String(reinterpret_cast<const char*>(glGetString(GL_VERSION))));
        qDebug() << "v: " << versionString;
    }

    
    ++m_frame;
}

void yage::GLWindow::endDraw ( void )
{
    if (!isExposed() || !m_context) {
        return;
    }
    m_context->swapBuffers( this );
    m_context->doneCurrent();
}

void yage::GLWindow::makeCurrent ( void )
{
    if (m_context) {
        m_context->makeCurrent( this );
    }
}


//-------------------------------------------------------------------------------------------------
// C-Bindings

GRAPHICSCORE_API yage::GLWindow* createGLWindow  (void)
{
    return new yage::GLWindow;    
}

GRAPHICSCORE_API void deleteGLWindow (yage::GLWindow *glw)
{
    delete glw;
}

GRAPHICSCORE_API void resize (yage::GLWindow *glw, int width, int height)
{
    glw->resize(width, height);
}

GRAPHICSCORE_API int height ( yage::GLWindow *glw )
{
    return glw->height();
}

GRAPHICSCORE_API int width ( yage::GLWindow *glw )
{
    return glw->width();
}

GRAPHICSCORE_API double pixelRatio ( yage::GLWindow *glw )
{
    return glw->pixelRatio();
}

GRAPHICSCORE_API void show( yage::GLWindow *glw )
{
    glw->show();
}

GRAPHICSCORE_API void beginDraw ( yage::GLWindow *glw )
{
    glw->beginDraw();
}

GRAPHICSCORE_API void endDraw ( yage::GLWindow *glw )
{
    glw->endDraw();
}

GRAPHICSCORE_API void makeCurrent ( yage::GLWindow *glw )
{
    glw->makeCurrent();
}


