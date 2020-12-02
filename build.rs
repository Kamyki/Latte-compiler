extern crate lalrpop;

fn main() {
    lalrpop::Configuration::new().log_verbose().emit_comments(true).process_current_dir().unwrap()
}