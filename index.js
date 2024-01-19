// AWS Lambda handler that returns a response 
exports.handler = async (event, context) => {
    // TODO implement
    const response = {
        statusCode: 200,
        body: JSON.stringify('Hello from Lambda!'),
    };
    return response;
};
