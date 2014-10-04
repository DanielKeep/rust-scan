#![feature(managed_boxes)]
#![feature(phase)]
#![feature(plugin_registrar)]
#![feature(quote)]
#![feature(struct_variant)]

#[phase(plugin, link)] extern crate log;
extern crate rustc;
extern crate syntax;

extern crate scan_util;

pub mod parse;
pub mod plugin;

#[plugin_registrar]
pub fn plugin_registrar(reg: &mut rustc::plugin::Registry) {
	reg.register_macro("scan", plugin::expand_scan);
	reg.register_macro("scanln", plugin::expand_scanln);
}
