interface IProgressBar {
    progress: number;
}

const ProgressBar = ({ progress }: IProgressBar) => {
    return (
        <div className="w-full overflow-hidden h-2 mb-4 text-xs flex rounded bg-gray-200">
            <div
                style={{ width: `${progress}%` }}
                className="shadow-none flex flex-col text-center whitespace-nowrap text-white justify-center bg-green-500"
            ></div>
        </div>
    );
};

export default ProgressBar;
