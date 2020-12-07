if [ -d ~/.cargo ]; then
	rustc --version
else
	curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- --no-modify-path
fi
