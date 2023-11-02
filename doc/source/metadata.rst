.. _METADATA:

Marmalade NG Metadata
=====================
The URI of a token (as retrieved by :ref:`LEDGER-GET-URI`) must point to a JSON off-chain Metadata Object.

Ideally the Object is stored on ipfs. (``ipfs://CID``).

A token metadata has the following format:

.. list-table:: JSON Metadata
  :widths: 25 15 60
  :header-rows: 1

  * - Field Name
    - Data Type
    - Description

  * - name
    - string
    - Identifies the asset to which this NFT represents.

  * - description
    - string
    - Describes the asset to which this NFT represents.

  * - image
    - string
    - | A URI pointing to a resource with mime type ``image/*`` representing the asset
      | to which this NFT represents. Consider making any images at a width between
      | 320 and 1080 pixels and aspect ratio between 1.91:1 and 4:5 inclusive.

  * - | attributes
      | *(Optionnal)*
    - array of objects
    - | An array of objects, each representing a characteristic or trait of the
      | NFT, with a *trait_type* field specifying the trait's name and a *value*
      | field specifying the trait's value.

  * - | properties
      | *(Optionnal, see below)*
    - object
    - Arbitrary properties. Values may be strings, numbers, object or arrays.


.. list-table:: JSON Metadata (properties object)
  :widths: 25 15 60
  :header-rows: 1

  * - Field Name
    - Data Type
    - Description

  * - | authors
      | *(Optionnal)*
    - array of objects
    - | An array of authors who created or contributed to the asset. Each author
      | is an object with a *name* field specifying the author's name.

  * - | external_url
      | *(Optionnal)*
    - string
    - URL to an external application or website where users can also view the asset.

  * - | animation_url
      | *(Optionnal)*
    - string
    - | URL to a multimedia attachment of the asset. The supported file formats are
      | MP4 and MOV for video, MP3, FLAC and WAV for audio, GLB for AR/3D assets,
      | and HTML for HTML pages. You may use the ``?ext={file_extension}`` query to
      | provide information on the file type.

  * - | collection
      | *(Optionnal)*
    - object
    - | An object with a *name* field specifying the name of the collection, and a
      | *family* field specifying the larger category or group to which the collection belongs.
