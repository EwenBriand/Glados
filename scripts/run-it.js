const fs = require('fs').promises;
const util = require('util');
const exec = util.promisify(require('child_process').exec);
const path = require('path');

// Colors BABY
const red = '\x1b[31m%s\x1b[0m';
const green = '\x1b[32m%s\x1b[0m';
const yellow = '\x1b[33m%s\x1b[0m';

// Tests directory
const directoryPath = './tests';

// Define the path to your executable
const executablePath = './glados -e -s';

// Tests File Extension
const fileExtension = '.gld';

// Hashmap tests output funciton
const testsOutput = {
  './tests/call_recursive_function.gld': '404',
  './tests/new_language_closure.gld' : '',
  './tests/new_language_for.gld': '22',
  './tests/new_language_if.gld': '7',
  './tests/new_language_lambda_with_params.gld': '3',
  './tests/new_language_struct.gld': 'false',
  './tests/new_language_while.gld': '22',
  './tests/ptr.gld': 'true',
  './tests/simpleAdd.gld': '',
  './tests/simpleFunCall.gld': '',
  './tests/simpleFuncCallWithArgs.gld': '',
  './tests/syscall.gld': '',
  './tests/test_simple_sum.gld': '',
  './tests/write_test.gld': '',
  // './tests/includes/code2.gld': '5',
  './tests/test_call_fun_no_args.gld': '3',
  './tests/test_call_fun_with_args.gld': '2',
  './tests/test_call_if_eq.gld': '6',
  './tests/test_call_if_inf.gld': '6',
  './tests/test_call_if_infeq.gld': '6',
  './tests/test_call_if_sup.gld': '6',
  './tests/test_call_if_supeq.gld': '6',
  './tests/test_call_if_true.gld': '6',
  './tests/test_call_lambda.gld': '0',
  './tests/test_call_lambda_with_arguments.gld': '24',
  './tests/test_call_lambda_with_variable.gld': '24',
  './tests/test_call_varriable.gld': '3',
  './tests/tests_from_pdf/builtins1.gld': '11',
  './tests/tests_from_pdf/builtins2.gld': '1',
  './tests/tests_from_pdf/builtins3.gld': '0',
  './tests/tests_from_pdf/call.gld': '5',
  './tests/tests_from_pdf/error.gld': 'Error was expected',
  './tests/tests_from_pdf/factorial.gld': '6',
  './tests/tests_from_pdf/foo.gld': '42',
  './tests/tests_from_pdf/function1.gld': '7',
  './tests/tests_from_pdf/if1.gld': '1',
  './tests/tests_from_pdf/if2.gld': '2',
  './tests/tests_from_pdf/if3.gld': '21',
  './tests/tests_from_pdf/lambda1.gld': '3',
  './tests/tests_from_pdf/lambda2.gld': '7',
  './tests/tests_from_pdf/superior.gld': 'true',
};

function parseLastLine(input) {
  // Split the input text into lines
  const lines = input.split('\n');

  if (lines.length < 2)
    return input;
  // Take the last line and remove any leading/trailing whitespace
  const lastLine = lines[lines.length - 2].trim();

  return lastLine;
}

async function processFile(pathToFile) {
  try {
    // Do a bash command with the path to file and the executable binary
    const { stdout } = await exec(`${executablePath} ${pathToFile}`);

    return stdout;
  } catch (err) {
    if (err.code === 84) {
      return 'Error was expected';
    }
    console.error('Error:', err);
    process.exit(1);
  }
}

async function checkTestsNumber() {
  const testsNumber = Object.keys(testsOutput).length;
  let matchingFilesCounts = 0;

  try {
    let files = await fs.readdir(directoryPath);
    matchingFilesCounts = (files.filter(file => path.extname(file) === fileExtension)).length;
    files = await fs.readdir(`${directoryPath}/tests_from_pdf`);
    matchingFilesCounts += (files.filter(file => path.extname(file) === fileExtension)).length;
  } catch (err) {
    console.error('Error reading directory:', err);
  }

  if (testsNumber !== matchingFilesCounts) {
    console.log(yellow, `WARNING: ${testsNumber} tests were expected but ${matchingFilesCounts} were found\n`);
  } else {
    console.log(green, `${testsNumber} tests were found\n`);
  }
}

async function runIntegrationTests () {
  let testFailed = 0;
  console.log('\nTesting is about to start...\n');

  // Check Tests numbers
  await checkTestsNumber();

  // Run all the tests
  for( const testName in testsOutput ) {
    const expectedOutput = testsOutput[testName];

    await processFile(testName)
      .then((output) => {
        output = parseLastLine(output);
        if (output === expectedOutput) {
          console.log(`Test ${testName} passed!`);
        } else {
          console.log(red, `Test ${testName} failed!\n"${output}" does not match expected "${expectedOutput}"`);
          testFailed++;
        }
      });
    }
    if (testFailed) {
      console.log(red, `\n${testFailed} tests failed! Please check them\n`);
      process.exit(1);
    } else {
      console.log(green, '\nAll tests passed!\n');
      process.exit(0);
    }
}

runIntegrationTests();