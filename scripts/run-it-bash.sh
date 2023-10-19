#!/bin/sh

# Check if Node.js is installed
if ! [ -x "$(command -v node)" ]; then
  echo "Node.js is not installed. Installing..."

  # Install Node.js using a package manager, for example, using Node Version Manager (nvm)
  # Replace with the appropriate installation method for your system if needed
  curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.38.0/install.sh | bash
  source ~/.nvm/nvm.sh  # Load nvm
  nvm install node  # Install the latest version of Node.js

  # Check if the installation was successful
  if [ -x "$(command -v node)" ]; then
    echo "Node.js has been installed successfully."
  else
    echo "Failed to install Node.js. Please install Node.js manually."
    exit 1
  fi
else
  echo "Node.js is already installed."
fi

# Run the JS Integration tests
node scripts/run-it.js
