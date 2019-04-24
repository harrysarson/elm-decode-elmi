const { Elm } = require('./elm');
const fs = require('fs').promises;
const path = require('path');

const app = Elm.Main.init();

(async () => {
    // const binary = await fs.readFile(path.join(
    //         __dirname,
    //         '..',
    //         '..',
    //         'complex',
    //         'elm-stuff',
    //         'generated-code',
    //         'elm-explorations',
    //         'test',
    //         'elm-stuff',
    //         '0.19.0',
    //         'ComplexSpec.elmi',
    //     ));
    const ifaces = await fs.readFile(path.join(
        process.env.HOME,
        '.elm',
        '0.19.0',
        'package',
        'elm',
        'core',
        '1.0.2',
        'ifaces.dat',
    ));

    app.ports.scanIfacesDat.send(ifaces.toString('base64'));
})();
