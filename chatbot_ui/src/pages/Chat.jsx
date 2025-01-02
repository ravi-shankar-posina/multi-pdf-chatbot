import { Button } from '@nextui-org/button';
import { MicIcon, SendHorizonalIcon, SendIcon } from 'lucide-react';
import React, { useEffect, useMemo, useState } from 'react';
import ReactMarkdown from 'react-markdown';
import remarkGfm from 'remark-gfm';

const Chat = ({ api }) => {
    const [inputMessage, setInputMessage] = useState('');
    const [currentQuestion, setCurrentQuestion] = useState(null);
    const [currentResponse, setCurrentResponse] = useState(null);
    const [selectedOption, setSelectedOption] = useState(api);
    const [isLoading, setIsLoading] = useState(false);
    const [showInitialInput, setShowInitialInput] = useState(true);
    const [source, setSource] = useState();
    const [content, setContent] = useState([]);
    const [links, setLinks] = useState([]);
    const [images, setImages] = useState([]);
    const [showSatisfactionQuestion, setShowSatisfactionQuestion] = useState(false);
    const [thankYouMessage, setThankYouMessage] = useState('');
    const [additionalResponse, setAdditionalResponse] = useState('');

    const [audioContext, setAudioContext] = useState(null);
    const [listenActive, setListenActive] = useState(false);

    const [isIdle, setIsIdle] = useState(false);
    const [idleTimeout, setIdleTimeout] = useState(null);

    const ref = React.useRef(idleTimeout);

    const renewIdle = useMemo(() => {
        console.log('renew idle-----------------');
        ref.current = idleTimeout;
        return () => {
            setIsIdle(false);

            clearTimeout(ref.current);
        };
    }, [idleTimeout]);

    const startVoiceOver = async () => {
        const SpeachRecognition = window.SpeechRecognition || window.webkitSpeechRecognition;
        const stt = new SpeachRecognition();
        stt.continuous = true;
        stt.lang = 'en-IN';
        stt.interimResults = true;
        stt.maxAlternatives = 1;
        setAudioContext(stt);

        const SpeechGrammarList = window.SpeechGrammarList || window.webkitSpeechGrammarList;
        // Add LEXA to grammar
        stt.grammars = new SpeechGrammarList();
        stt.grammars.addFromString('#JSGF V1.0; grammar lexa; public <lexa> = lexa;', 1);

        stt.onresult = function (event) {
            console.log(event);
            let speechToText = '';
            // let speechToText = event.results[event.results.length - 1][0].transcript;
            let recog = [];
            for (let i = 0; i < event.results.length; i++) {
                recog.push(event.results[i][0].transcript);
            }
            speechToText = recog.join(' ');
            console.log(speechToText);
            setInputMessage(speechToText);
            renewIdle();
            setIdleTimeout(
                setTimeout(() => {
                    console.log('idle fired');

                    setIsIdle(true);
                }, 5000)
            );
        };
        stt.onsoundstart = function () {
            console.log('stt sound start');
        };
        stt.onsoundend = function () {
            console.log('stt sound end');
        };
        stt.onend = function () {
            console.log('stt end');
            setListenActive(false);
        };
        stt.onerror = function (event) {
            console.log(event);
            setListenActive(false);
            stt.stop();
            // stt.start();
        };
        stt.start();

        setListenActive(true);
    };

    const handleSendMessage = async () => {
        if (inputMessage.trim() === '') return;

        setCurrentQuestion(inputMessage);
        setShowInitialInput(false);
        setInputMessage('');
        setCurrentResponse(' ');
        setContent([]);
        setLinks([]);
        setImages([]);
        setIsLoading(true);
        setShowSatisfactionQuestion(false);
        setThankYouMessage('');
        setAdditionalResponse('');

        try {
            const response = await fetch(`${import.meta.env.VITE_API_URL}/${selectedOption}`, {
                method: 'POST',
                headers: {
                    'Content-Type': 'application/json',
                },
                body: JSON.stringify({ query: inputMessage }),
            });

            if (!response.ok) {
                throw new Error(`Network response was not ok: ${response.statusText}`);
            }

            const text = await response.text();
            const data = JSON.parse(text);
            const sources = data.sources || [];

            if (sources.length > 0) {
                const updatedContent = sources.map((source) => source.page_content || ' ');
                setContent(updatedContent);
            } else {
                setContent([]);
            }

            // const img = data.images || [];
            // if (img.length > 0) {
            //   setImages(img.map((image) => `data:image/png;base64,${image}`));
            // } else {
            //   setImages([]);
            // }

            setCurrentResponse(data.answer || ' ');
            setLinks(data.links || []);
            setShowSatisfactionQuestion(true);
        } catch (error) {
            console.error('Error fetching data:', error);
            setCurrentResponse('Error fetching response.');
        } finally {
            setIsLoading(false);
        }
    };
    useEffect(() => {
        if (listenActive && isIdle) {
            if (inputMessage.trim() === '') {
                return;
            }
            // setMessages((prev) => {
            //     let resp = [
            //         ...prev,
            //         {
            //             type: 'user',
            //             text: text,
            //         },
            //     ];
            //     socket.emit('lexa', {
            //         type: 'text',
            //         data: resp,
            //     });
            //     return resp;
            // });
            let msgs = [];
            console.log('idle fired');

            // msgs = [
            //     ...msgs,
            //     {
            //         type: 'user',
            //         text: text,
            //     },
            // ];

            audioContext.stop();
            setListenActive(false);
            // setProcessing(true);
        }
    }, [isIdle]);
    return (
        <div className="flex h-full bg-white relative break-words ">
            <div className="flex-1 flex flex-col relative">
                {isLoading && (
                    <div className="absolute inset-0 bg-gray-100 bg-opacity-75 flex justify-center items-center z-10">
                        <div className="animate-spin rounded-full h-12 w-12 border-b-4 border-green-700"></div>
                    </div>
                )}

                <div className="flex-1 overflow-y-auto bg-white">
                    <div className="h-full flex flex-col">
                        <div className="flex-1 flex flex-col">
                            {showInitialInput ? (
                                <div className="flex-1 flex items-center justify-center">
                                    <div className="w-full px-2">
                                        <input
                                            type="text"
                                            value={inputMessage}
                                            onChange={(e) => setInputMessage(e.target.value)}
                                            placeholder="Type your question..."
                                            className="w-[90%] ml-20 h-24 border border-gray-300 rounded-lg px-4 py-2 focus:outline-none focus:ring-2 focus:ring-green-700 text-black"
                                            onKeyPress={(e) => {
                                                if (e.key === 'Enter') {
                                                    e.preventDefault();
                                                    handleSendMessage();
                                                }
                                            }}
                                        />
                                        <div className="flex items-center justify-center my-2">
                                            <div className="flex gap-2  py-2 px-2 rounded-full w-min justify-center">
                                                <Button
                                                    id="voice-activation"
                                                    size="lg"
                                                    className={`rounded-full h-16 w-16 ${listenActive ? 'bg-red-500 animate-pulse' : 'bg-gray-400'}`}
                                                    isIconOnly
                                                    onClick={() => {
                                                        if (listenActive) {
                                                            audioContext.stop();
                                                            // stopVoiceOver();
                                                        } else {
                                                            startVoiceOver();
                                                        }
                                                    }}
                                                    // isLoading={listenActive}
                                                >
                                                    <MicIcon size={28} />
                                                </Button>
                                            </div>
                                            <div className="flex gap-2  py-2 px-2 rounded-full w-min justify-center">
                                                <Button
                                                    id="voice-activation"
                                                    size="lg"
                                                    className={`rounded-full h-16 w-16 bg-gray-400`}
                                                    isIconOnly
                                                    onClick={() => {
                                                        handleSendMessage();
                                                    }}
                                                    // isLoading={listenActive}
                                                >
                                                    <SendHorizonalIcon size={28} />
                                                </Button>
                                            </div>
                                        </div>
                                        {/* <button
                                            className="ml-2 px-4 py-2 bg-green-500 text-white rounded-lg"
                                            onClick={() => {
                                                if (!listenActive) {
                                                    startVoiceOver();
                                                } else {
                                                    audioContext.stop();
                                                }
                                            }}
                                        >
                                            {listenActive ? 'Stop' : 'Listen'}
                                        </button> */}
                                    </div>
                                </div>
                            ) : (
                                <div className="flex-1 flex flex-col p-8 overflow-y-auto max-h-[calc(100vh-120px)]">
                                    <div className="max-w-3xl w-full mx-auto">
                                        {currentQuestion && (
                                            <div className="mb-4">
                                                <div className="text-black font-bold text-xl whitespace-normal">
                                                    <h1>{currentQuestion}</h1>
                                                </div>
                                            </div>
                                        )}
                                        {additionalResponse && (
                                            <div className="mb-4">
                                                <h1 className="py-2 font-bold ">Response genarated by LLM</h1>
                                                <ReactMarkdown className="markdown-body" remarkPlugins={[remarkGfm]}>
                                                    {additionalResponse}
                                                </ReactMarkdown>
                                            </div>
                                        )}
                                        {currentResponse && (
                                            <div className="mb-4">
                                                {selectedOption === 'analyze' ? (
                                                    <div
                                                        dangerouslySetInnerHTML={{
                                                            __html: currentResponse,
                                                        }}
                                                    />
                                                ) : (
                                                    <ReactMarkdown className="markdown-body" remarkPlugins={[remarkGfm]}>
                                                        {currentResponse}
                                                    </ReactMarkdown>
                                                )}
                                            </div>
                                        )}
                                        {images.length > 0 && (
                                            <div className="mb-2 flex flex-col">
                                                {images.map((image, index) => (
                                                    <img key={index} src={image} alt={`Fetched ${index}`} className="max-w-full h-auto rounded-lg" />
                                                ))}
                                            </div>
                                        )}

                                        {content.length > 0 && (
                                            <div className="mt-4">
                                                <h2 className="font-bold text-lg">Related Information:</h2>
                                                <ul className="list-disc pl-5">
                                                    {content.map((item, index) => (
                                                        <li key={index}>{item.replace(/^prompt:\s*/, '')}</li>
                                                    ))}
                                                </ul>
                                            </div>
                                        )}
                                        {links.length > 0 && (
                                            <div className="mt-4">
                                                <h2 className="font-bold text-lg">Related Links:</h2>
                                                <ul className="list-disc ml-5 mt-2">
                                                    {links.map((link, index) => (
                                                        <li key={index}>
                                                            <a href={link} target="_blank" rel="noopener noreferrer" className="text-blue-500 underline">
                                                                {link}
                                                            </a>
                                                        </li>
                                                    ))}
                                                </ul>
                                            </div>
                                        )}
                                    </div>
                                </div>
                            )}
                            {!showInitialInput && (
                                <div className="p-2 bg-gray-50 border-t border-gray-200">
                                    <div className="flex space-x-2">
                                        <input
                                            type="text"
                                            value={inputMessage}
                                            onChange={(e) => setInputMessage(e.target.value)}
                                            placeholder="Type your question..."
                                            className="flex-1 border border-gray-300 rounded-lg px-4 py-2 focus:outline-none focus:ring-2 focus:ring-green-700 text-black"
                                            onKeyPress={(e) => {
                                                if (e.key === 'Enter') {
                                                    e.preventDefault();
                                                    handleSendMessage();
                                                }
                                            }}
                                        />
                                        <div className="flex gap-2 bg-gray-300 py-2 px-2 rounded-full h-12 w-12">
                                            <Button
                                                id="voice-activation"
                                                size="sm"
                                                className={`rounded-full h-8 w-8 ${listenActive ? 'bg-red-500 animate-pulse' : 'bg-gray-400'}`}
                                                isIconOnly
                                                onClick={() => {
                                                    if (listenActive) {
                                                        audioContext.stop();
                                                        // stopVoiceOver();
                                                    } else {
                                                        startVoiceOver();
                                                    }
                                                }}
                                                // isLoading={listenActive}
                                            >
                                                <MicIcon size={18} />
                                            </Button>
                                        </div>
                                        {/* <button
                                            className="ml-2 px-4 py-2 bg-green-500 text-white rounded-lg"
                                            onClick={() => {
                                                if (!listenActive) {
                                                    startVoiceOver();
                                                } else {
                                                    audioContext.stop();
                                                }
                                            }}
                                        >
                                            {listenActive ? 'Stop' : 'Listen'}
                                        </button> */}
                                        <div className="flex gap-2 bg-gray-300 py-2 px-2 rounded-full h-12 w-12">
                                            <Button
                                                id="voice-activation"
                                                size="sm"
                                                className={`rounded-full h-8 w-8 bg-gray-400`}
                                                isIconOnly
                                                onClick={() => {
                                                    handleSendMessage();
                                                }}
                                                // isLoading={listenActive}
                                            >
                                                <SendHorizonalIcon size={18} />
                                            </Button>
                                        </div>
                                    </div>
                                </div>
                            )}
                        </div>
                    </div>
                </div>
            </div>
        </div>
    );
};

export default Chat;
