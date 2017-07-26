extern crate lalrpop;

fn main() {
  let mut cfg = lalrpop::Configuration::new();
  cfg.log_verbose();
  cfg.process_current_dir().unwrap();
}
