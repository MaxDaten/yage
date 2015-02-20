#ifndef __VOXELIZE_H__
#define __VOXELIZE_H__

bool isVoxelPresent(in vec4 voxel)
{
  return dot(voxel,voxel) > 0;
}

#endif /* __VOXELIZE_H__ */
