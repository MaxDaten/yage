#pragma once

#ifdef _WIN32
#ifdef GRAPHICSCORE_EXPORTS
	#define GRAPHICSCORE_API __declspec(dllexport) 
	#else
	#define GRAPHICSCORE_API __declspec(dllimport) 
#endif
#else
	#define GRAPHICSCORE_API 
#endif
