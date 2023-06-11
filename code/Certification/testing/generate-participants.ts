function makeParticipant(n: number) {
    return {
    "id": n,
    "name": "Name"+n,
    "pubkeyhash": "27fa9b2ac96b35829b84f16a913f1e22fabd4a792ab0b7537f9a74a8",
    "email": null,
    "examCBOR": "49480100002221200101"
}
}

const arrayOfParticipants = Array.from(Array(8008).keys()).map(x => makeParticipant(x))

const arrayOfParticipantsJSON = JSON.stringify(arrayOfParticipants);
// write the data to the data folder.
Deno.writeTextFileSync("./testing/participants.json", arrayOfParticipantsJSON);