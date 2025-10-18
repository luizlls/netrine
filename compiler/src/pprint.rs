use std::borrow::Cow;
use std::fmt::{self, Display};

use crate::state::State;

pub struct PrettyPrintNode<'pp> {
    label: Option<Cow<'pp, str>>,
    children: Vec<PrettyPrintChild<'pp>>,
}

impl<'pp> PrettyPrintNode<'pp> {
    fn write(&self, f: &mut fmt::Formatter<'_>, state: &State, mut depth: usize) -> fmt::Result {
        if let Some(label) = &self.label {
            writeln!(f, "{}{}", "  ".repeat(depth), label)?;
            depth += 1;
        };

        for child in &self.children {
            match child {
                PrettyPrintChild::Label(s) => s.print(state).write(f, state, depth)?,
                PrettyPrintChild::Printer(p) => p.print(state).write(f, state, depth)?,
            }
        }

        Ok(())
    }

    pub fn printer() -> PrettyPrintNodeBuilder<'pp> {
        PrettyPrintNodeBuilder::new()
    }
}

enum PrettyPrintChild<'pp> {
    Label(Cow<'pp, str>),
    Printer(&'pp dyn PrettyPrint),
}

pub struct PrettyPrintNodeBuilder<'pp> {
    label: Option<Cow<'pp, str>>,
    children: Vec<PrettyPrintChild<'pp>>,
}

impl<'pp> PrettyPrintNodeBuilder<'pp> {
    fn new() -> PrettyPrintNodeBuilder<'pp> {
        PrettyPrintNodeBuilder {
            label: None,
            children: vec![],
        }
    }

    pub fn label(mut self, label: impl Into<Cow<'pp, str>>) -> PrettyPrintNodeBuilder<'pp> {
        self.label = Some(label.into());
        self
    }

    pub fn child(mut self, child: &'pp dyn PrettyPrint) -> PrettyPrintNodeBuilder<'pp> {
        self.add_child(child);
        self
    }

    pub fn add_child(&mut self, child: &'pp dyn PrettyPrint) {
        self.children.push(PrettyPrintChild::Printer(child));
    }

    pub fn add_label(&mut self, child: impl Into<Cow<'pp, str>>) {
        self.children.push(PrettyPrintChild::Label(child.into()));
    }

    pub fn print(self) -> PrettyPrintNode<'pp> {
        PrettyPrintNode {
            label: self.label,
            children: self.children,
        }
    }
}

pub trait PrettyPrint {
    fn print(&self, state: &State) -> PrettyPrintNode<'_>;

    fn pprint<'pp>(&'pp self, state: &'pp State) -> PrettyPrintWithState<'pp>
    where
        Self: Sized,
    {
        PrettyPrintWithState::new(self, state)
    }
}

pub struct PrettyPrintWithState<'pp> {
    state: &'pp State,
    printer: &'pp dyn PrettyPrint,
}

impl<'pp> PrettyPrintWithState<'pp> {
    fn new(printer: &'pp dyn PrettyPrint, state: &'pp State) -> PrettyPrintWithState<'pp> {
        PrettyPrintWithState { printer, state }
    }
}

impl Display for PrettyPrintWithState<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let state = self.state;
        self.printer.print(state).write(f, state, 0)
    }
}

impl<T: ?Sized + AsRef<str>> PrettyPrint for T {
    fn print(&self, _state: &State) -> PrettyPrintNode<'_> {
        PrettyPrintNode::printer().label(self.as_ref()).print()
    }
}
