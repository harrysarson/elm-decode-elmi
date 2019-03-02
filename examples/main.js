const {Elm} = require('../elm');
const fs = require('fs').promises;
const path = require('path');

(async () => {
    const binary = await fs.readFile(path.join(
            __dirname,
            '..',
            'elm-stuff',
            '0.19.0',
            'Parser-Elmi.elmi',
        ));

    Elm.Main.init({flags: {elmi: binary.toString('base64')}});

})();
