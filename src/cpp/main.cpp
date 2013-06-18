
#include "Application.hpp"
#include "GLWindow.hpp"
#include <QtCore/QDebug>

#include <math.h>

extern "C" {
	void realMain(yage::GLWindow *win, yage::Application *app);
}

int main (int argc, char** argv)
{
	realMain(0, 0);

	return 0;
}

void realMain(yage::GLWindow *inwin = 0, yage::Application *inapp = 0)
{
	if (!inapp){
		inapp = createApplication(0, 0);
	}
	if (!inwin){
		inwin = createGLWindow();
	}

	inwin->resize(800, 600);
	inwin->show();

	int i = 0;
	while (true)
	{
		inapp->processEvents();
		inwin->beginDraw();

		inwin->endDraw();
		++i;
	}
}
