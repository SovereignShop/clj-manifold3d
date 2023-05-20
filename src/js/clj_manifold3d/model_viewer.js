import { Document, WebIO, Accessor, Primitive, Node, Mesh, Scene } from '@gltf-transform/core';

var objectURL = null;

export function createGLTF(manifoldMesh) {

  const vertProps = manifoldMesh.vertProperties;
  const triVerts = manifoldMesh.triVerts;

  const doc = new Document();

  const buffer = doc.createBuffer('default');

  const positionAccessor = doc
        .createAccessor('position')
        .setBuffer(buffer)
        .setArray(new Float32Array(vertProps))
        .setType(Accessor.Type.VEC3);

  const indexAccessor = doc
        .createAccessor('index')
        .setBuffer(buffer)
        .setArray(new Uint32Array(triVerts))
        .setType(Accessor.Type.SCALAR);

  const prim = doc
        .createPrimitive()
        .setAttribute('position', positionAccessor)
        .setIndices(indexAccessor);

  const mesh = doc.createMesh('mesh').addPrimitive(prim);

  const node = doc.createNode('node').setMesh(mesh);

  const scene = doc.createScene('scene').addChild(node);

  doc.getRoot().setDefaultScene(scene);

  const io = new WebIO();
  const gltf = io.writeBinary(doc);

  gltf.then(gltfArrayBuffer => {

    const blob = new Blob([gltfArrayBuffer], { type: 'model/gltf-binary' });
    URL.revokeObjectURL(objectURL);
    objectURL = URL.createObjectURL(blob);

    const elem = document.querySelector('model-viewer')

    // Now you can use 'objectURL' as the src for a <model-viewer> element
    elem.src = objectURL;

  });
}
