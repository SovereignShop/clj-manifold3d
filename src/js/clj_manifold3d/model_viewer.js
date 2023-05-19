//import { GLTFExporter } from './GLTFExporter';
import { BufferGeometry, MeshStandardMaterial, Mesh, Float32BufferAttribute, Int16BufferAttribute } from 'three';
import { GLTFExporter } from 'three/examples/jsm/exporters/GLTFExporter';

import { Document, NodeIO, Primitive, Accessor, Mesh } from '@gltf-transform/core'


export async function pushMesh(mesh) {

  let geometry = new BufferGeometry();
  geometry.setAttribute('position', new  Float32BufferAttribute(mesh.vertProperties, 3));
  geometry.setIndex(new Int16BufferAttribute(mesh.triVerts, 3));

  console.log(mesh.vertProperties);
  console.log(mesh.triVerts);

  const material = new  MeshStandardMaterial({ color: 0x00ff00 });
  const threeMesh = new  Mesh(geometry, material);

  console.log("geometry:", geometry.index.array);

  // Now let's export this mesh as a GLTF
  const exporter = new GLTFExporter();

  exporter.parse(threeMesh, function (gltf) {
    const modelViewerElement = document.querySelector('#myModelViewer');

    const blob = new Blob([gltf], { type: 'model/gltf-binary' });
    const url = URL.createObjectURL(blob);


    modelViewerElement.src = url;
  }, function(error) {
    console.log("error:", error);
  }, { binary: true });

}




//
//export async function push_mv(manifold) {
//  const mv = document.querySelector('model-viewer');
//  let objectURL = null;
//
//  // From Z-up to Y-up (glTF)
//  const mesh = manifold.rotate([-90, 0, 0]).getMesh();
//
//  for (const [i, id] of mesh.runOriginalID.entries()) {
//    const indices = materialMap.get(id);
//    indices.setArray(mesh.triVerts.subarray(mesh.runIndex[i], mesh.runIndex[i + 1]));
//  }
//
//  const numVert = mesh.numVert;
//  const numProp = mesh.numProp;
//  const posArray = new Float32Array(3 * numVert);
//  const uvArray = new Float32Array(2 * numVert);
//  for (let i = 0; i < numVert; ++i) {
//    posArray[3 * i] = mesh.vertProperties[numProp * i];
//    posArray[3 * i + 1] = mesh.vertProperties[numProp * i + 1];
//    posArray[3 * i + 2] = mesh.vertProperties[numProp * i + 2];
//    uvArray[2 * i] = mesh.vertProperties[numProp * i + 3];
//    uvArray[2 * i + 1] = mesh.vertProperties[numProp * i + 4];
//  }
//  position.setArray(posArray);
//  uv.setArray(uvArray);
//
//  await texturesLoad;
//
//  const glb = await io.writeBinary(doc);
//
//  const blob = new Blob([glb], { type: 'application/octet-stream' });
//  URL.revokeObjectURL(objectURL);
//  objectURL = URL.createObjectURL(blob);
//  mv.src = objectURL;
//}
