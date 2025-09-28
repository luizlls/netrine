use std::fmt;

pub struct PrettyPrintNode<'pp> {
    label: Box<str>,
    children: Vec<&'pp dyn PrettyPrint>,
}

impl<'pp> PrettyPrintNode<'pp> {
    fn print(&self, f: &mut fmt::Formatter<'_>, depth: usize) -> fmt::Result {
        writeln!(f, "{}{}", "  ".repeat(depth), self.label)?;

        for child in &self.children {
            child.print().print(f, depth + 1)?;
        }

        Ok(())
    }

    pub fn printer() -> PrettyPrintNodeBuilder<'pp> {
        PrettyPrintNodeBuilder::new()
    }
}

pub struct PrettyPrintNodeBuilder<'pp> {
    label: Option<Box<str>>,
    children: Vec<&'pp dyn PrettyPrint>,
}

impl<'pp> PrettyPrintNodeBuilder<'pp> {
    fn new() -> PrettyPrintNodeBuilder<'pp> {
        PrettyPrintNodeBuilder {
            label: None,
            children: vec![],
        }
    }

    pub fn label(mut self, label: impl Into<Box<str>>) -> PrettyPrintNodeBuilder<'pp> {
        self.label = Some(label.into());
        self
    }

    pub fn child(mut self, child: &'pp dyn PrettyPrint) -> PrettyPrintNodeBuilder<'pp> {
        self.children.push(child);
        self
    }

    pub fn print(self) -> PrettyPrintNode<'pp> {
        PrettyPrintNode {
            label: self.label.expect("pretty print `label` is required"),
            children: self.children,
        }
    }
}

pub trait PrettyPrint {
    fn print(&self) -> PrettyPrintNode<'_>;
}

pub trait PrettyPrinter: PrettyPrint {
    fn pprint(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.print().print(f, 0)
    }
}

impl<T: ?Sized + PrettyPrint> PrettyPrinter for T {}
