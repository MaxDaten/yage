#pragma once

#include <QtGui/QWindow>
#include <QtCore/QDebug>
#include <QtGui/QOpenGLFunctions>

namespace yage {

class GRAPHICSCORE_API GLWindow : private QWindow, protected QOpenGLFunctions
{
public:
    GLWindow(QScreen *screen = (QScreen *)0);
    virtual ~GLWindow(void);

    void resize(int w, int h);
    int width ( void );
    int height ( void );
    void show (void);
    double pixelRatio ( void );

    void beginDraw ( void );
    void endDraw ( void );
    void makeCurrent ( void );

private:
    QOpenGLContext *m_context;
    int m_frame = 0;
};

} // namespace yage

//-------------------------------------------------------------------------------------------------
// C-Bindings

extern "C" {
    GRAPHICSCORE_API yage::GLWindow* createGLWindow ( void );
    GRAPHICSCORE_API void deleteGLWindow ( yage::GLWindow *glw );
    GRAPHICSCORE_API void resize ( yage::GLWindow *glw, int w, int h );
    GRAPHICSCORE_API int  width ( yage::GLWindow *glw );
    GRAPHICSCORE_API int  height ( yage::GLWindow *glw );
    GRAPHICSCORE_API double pixelRatio ( yage::GLWindow *glw );
    GRAPHICSCORE_API void show ( yage::GLWindow *glw );
    GRAPHICSCORE_API void beginDraw ( yage::GLWindow *glw );
    GRAPHICSCORE_API void endDraw ( yage::GLWindow *glw );
    GRAPHICSCORE_API void makeCurrent ( yage::GLWindow *glw );
}
