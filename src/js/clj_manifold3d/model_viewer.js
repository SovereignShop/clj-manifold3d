import { Document, WebIO, Accessor, Primitive, Node, Mesh, Scene } from '@gltf-transform/core';

var objectURL = null;

export function downloadBlob(url, filename) {

    // Create a link element
    const link = document.createElement('a');

    // Set properties of link
    link.href = url;
    link.download = filename;

    // This is necessary as link.click() does not work on the latest firefox
    link.style.display = 'none';
    document.body.appendChild(link);
    link.click();

    // Remove the link after download
    setTimeout(function () {
        document.body.removeChild(link);
        window.URL.revokeObjectURL(url);
    }, 100);
}

export function createGLTF(manifoldMesh) {

  const vertProps = manifoldMesh.vertProperties;
  const triVerts = manifoldMesh.triVerts;

  const doc = new Document();

  const buffer = doc.createBuffer('default');

  const material = doc.createMaterial('solidMaterial')
                      .setBaseColorFactor([1.0, 0.0, 0.0, 1.0]) // white color
                      .setAlphaMode('OPAQUE'); // ensure opaque rendering

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
        .setIndices(indexAccessor)
        .setMaterial(material);

  // Specify color attribute.
  let colors = [];
  for (let i = 0; i < vertProps.length / 3; i++) {
    colors.push(1.0, 0, 0);
  }


  prim.setAttribute('COLOR_0',
                    doc.createAccessor()
                       .setArray(new Float32Array(colors))
                       .setType('VEC3'));

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

    // downloadBlob(objectURL, "model.glb")

    const elem = document.querySelector('model-viewer')

    // Now you can use 'objectURL' as the src for a <model-viewer> element
    elem.src = objectURL;

  });
}

