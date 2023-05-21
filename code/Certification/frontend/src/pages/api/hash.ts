import type { NextApiRequest, NextApiResponse } from "next";
import * as blake2 from "blake2";

type Data = {
    hash: string;
};

export default function handler(
    req: NextApiRequest,
    res: NextApiResponse<Data>
) {
    // TODO: Check implementation
    const bin = new TextEncoder().encode(req.body.datum);
    const hashObj = blake2.createHash("blake2b").update(Buffer.from(bin));
    const hash = hashObj.digest("hex");
    // FIXME: This is a dummy implementation

    res.status(200).json({ hash: hash });
}
