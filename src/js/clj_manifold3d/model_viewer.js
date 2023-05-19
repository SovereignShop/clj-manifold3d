import * as THREE from 'three';

export async function push_mv(manifold) {
  const mv = document.querySelector('model-viewer');
  let objectURL = null;

  // From Z-up to Y-up (glTF)
  const mesh = manifold.rotate([-90, 0, 0]).getMesh();

  for (const [i, id] of mesh.runOriginalID.entries()) {
    const indices = materialMap.get(id);
    indices.setArray(mesh.triVerts.subarray(mesh.runIndex[i], mesh.runIndex[i + 1]));
  }

  const numVert = mesh.numVert;
  const numProp = mesh.numProp;
  const posArray = new Float32Array(3 * numVert);
  const uvArray = new Float32Array(2 * numVert);
  for (let i = 0; i < numVert; ++i) {
    posArray[3 * i] = mesh.vertProperties[numProp * i];
    posArray[3 * i + 1] = mesh.vertProperties[numProp * i + 1];
    posArray[3 * i + 2] = mesh.vertProperties[numProp * i + 2];
    uvArray[2 * i] = mesh.vertProperties[numProp * i + 3];
    uvArray[2 * i + 1] = mesh.vertProperties[numProp * i + 4];
  }
  position.setArray(posArray);
  uv.setArray(uvArray);

  await texturesLoad;

  const glb = await io.writeBinary(doc);

  const blob = new Blob([glb], { type: 'application/octet-stream' });
  URL.revokeObjectURL(objectURL);
  objectURL = URL.createObjectURL(blob);
  mv.src = objectURL;
}



