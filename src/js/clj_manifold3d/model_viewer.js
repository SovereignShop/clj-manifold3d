import { Document, WebIO, Accessor, Primitive, Node, Mesh, Scene, Material } from '@gltf-transform/core';

const io = new WebIO();

export function downloadURL(url, filename) {

    const link = document.createElement('a');

    link.download = filename;
    link.href = url;

    link.style.display = 'none';
    document.body.appendChild(link);
    link.click();

}

var objectURL = null;

export async function createGLTF(manifold) {

  const manifoldMesh = manifold.rotate([-90, 0, 0]).getMesh()

  const vertProps = manifoldMesh.vertProperties;
  const triVerts = manifoldMesh.triVerts;

  const doc = new Document();
  const buffer = doc.createBuffer();

  const material = doc.createMaterial('solidMaterial')
                      .setBaseColorFactor([1.0, 0.0, 0.0, 1.0]) // white color
                      .setDoubleSided(true)
                      .setAlphaMode('OPAQUE'); // ensure opaque rendering

  const positionAccessor = doc
        .createAccessor()
        .setBuffer(buffer)
        .setType(Accessor.Type.VEC3)
        .setArray(vertProps);

  const indexAccessor = doc
        .createAccessor()
        .setBuffer(buffer)
        .setType(Accessor.Type.SCALAR)
        .setArray(triVerts);

  const prim = doc
        .createPrimitive()
        .setMaterial(material)
        .setIndices(indexAccessor)
        .setAttribute('POSITION', positionAccessor);

  const mesh = doc.createMesh('mesh').addPrimitive(prim);

  const node = doc.createNode('result').setMesh(mesh);

  const scene = doc.createScene('scene').addChild(node);

  doc.getRoot().setDefaultScene(scene);

  const glb = await io.writeBinary(doc);

  const blob = new Blob([glb], {type: 'application/octet-stream'});
  //downloadURL(URL.createObjectURL(blob), "test.glb")

  URL.revokeObjectURL(objectURL);
  objectURL = URL.createObjectURL(blob);

  // downloadURL(objectURL, "model.glb")

  const elem = document.querySelector('model-viewer')

  // Now you can use 'objectURL' as the src for a <model-viewer> element
  elem.src = objectURL;
}
