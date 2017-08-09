doubleJacobianTestZEqualsOne :: Bool
doubleJacobianTestZEqualsOne = (x', y', z') == doubleJacobian (x, y, z)
    where
        x = 0x34f9460f0e4f08393d192b3c5133a6ba099aa0ad9fd54ebccfacdfa239ff49c6
        y = 0x0b71ea9bd730fd8923f6d25a7a91e7dd7728a960686cb5a901bb419e0f2ca232
        z = 1
        x' = 0xec9f153b13ee7bd915882859635ea9730bf0dc7611b2c7b0e37ee64f87c50c27
        y' = 0xb082b53702c466dcf6e984a35671756c506c67c2fcb8adb408c44dd0755c8f2a
        z' = 0x16e3d537ae61fb1247eda4b4f523cfbaee5152c0d0d96b520376833c1e594464


doubleJacobianTestZEqualsTwo :: Bool
doubleJacobianTestZEqualsTwo = (x', y', z') == doubleJacobian (x, y, z)
    where
        x = 0xd3e5183c393c20e4f464acf144ce9ae8266a82b67f553af33eb37e88e7fd2718
        y = 0x5b8f54deb987ec491fb692d3d48f3eebb9454b034365ad480dda0cf079651190
        z = 2
        x' = 0x9f153b13ee7bd915882859635ea9730bf0dc7611b2c7b0e37ee65073c50fabac
        y' = 0x2b53702c466dcf6e984a35671756c506c67c2fcb8adb408c44dd125dc91cb988
        z' = 0x6e3d537ae61fb1247eda4b4f523cfbaee5152c0d0d96b520376833c2e5944a11

addJacobiansTest :: Bool
addJacobiansTest =
    (x3, y3, z3) == addJacobians (x1, y1, z1) (x2, y2, z2)
    where
        x1 = 0x34f9460f0e4f08393d192b3c5133a6ba099aa0ad9fd54ebccfacdfa239ff49c6
        y1 = 0x0b71ea9bd730fd8923f6d25a7a91e7dd7728a960686cb5a901bb419e0f2ca232
        z1 = 1
        x2 = 0xd74bf844b0862475103d96a611cf2d898447e288d34b360bc885cb8ce7c00575
        y2 = 0x131c670d414c4546b88ac3ff664611b1c38ceb1c21d76369d7a7a0969d61d97d
        z2 = 1
        x3 = 0x0cfbc7da1e569b334460788faae0286e68b3af7379d5504efc25e4dba16e46a6
        y3 = 0xe205f79361bbe0346b037b4010985dbf4f9e1e955e7d0d14aca876bfa79aad87
        z3 = 0x44a5646b446e3877a648d6d381370d9ef55a83b666ebce9df1b1d7d65b817b2f
