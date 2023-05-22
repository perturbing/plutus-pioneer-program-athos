import type { NextApiRequest, NextApiResponse } from "next";
import {
    C,
    fromHex,
    toHex
} from "lucid-cardano";

type Data = {
    hash: string;
};

export default function handler(
    req: NextApiRequest,
    res: NextApiResponse<Data>
) {
    const bin = fromHex(req.body.datum);
    const hash = toHex(C.hash_blake2b256(bin));

    res.status(200).json({ hash: hash });
}
