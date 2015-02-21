#ifndef __VOXELIZE_H__
#define __VOXELIZE_H__

/*
Voxelize Mode
  0: voxelize scene
  1: voxelize page mask
*/
#define VOXELIZESCENE 0
#define VOXELPAGEMASK 1
uniform int VoxelizeMode;

bool isVoxelPresent(in vec4 voxel)
{
  return dot(voxel,voxel) > 0;
}


#endif /* __VOXELIZE_H__ */
