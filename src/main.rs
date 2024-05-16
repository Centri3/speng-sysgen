#![feature(if_let_guard)]

use aho_corasick::{AhoCorasick, MatchKind};
use anyhow::{bail, Context, Result};
use indoc::formatdoc;
use std::fs::File;
use std::io::prelude::*;
use std::marker::PhantomData;

const KM_TO_AU: f64 = 149597870.691f64;
const AU_TO_KM: f64 = 0.0000000066845871226705985f64;

const DAYS_TO_YEARS: f64 = 365.24218985f64;
const YEARS_TO_DAYS: f64 = 0.0027379093319166836f64;

mod sealed {
    pub trait Sealed {}

    impl Sealed for () {}
    impl Sealed for super::Km {}
    impl Sealed for super::Au {}
    impl Sealed for super::Days {}
    impl Sealed for super::Years {}
}

pub struct SubstsBuilder<'a, D: DistanceTy, P: PeriodTy>(Substs<'a, D, P>);

impl<'a, D: DistanceTy, P: PeriodTy> SubstsBuilder<'a, D, P> {
    /// Generates 1000 bodies by default with no system template or orbit
    /// increment
    fn new(gen_template: &'a str) -> Self {
        Self(Substs {
            sys_template: None,
            gen_template,
            num_of_bodies: 1000,
            orbit_increment: OrbitIncrement::No,
        })
    }

    /// Adds the system's template if it's desired. This is essentially the rest
    /// of the system. This is prepended to the file with no changes, so this
    /// should not have any substitutions.
    fn with_sys_template(&mut self, sys_template: &'a str) -> Self {
        Self(Substs {
            sys_template: Some(sys_template),
            ..self.0
        })
    }

    fn with_num_of_bodies(&mut self, num_of_bodies: i32) -> Self {
        Self(Substs {
            num_of_bodies,
            ..self.0
        })
    }

    fn with_orbit_increment(&mut self, orbit_increment: OrbitIncrement<D, P>) -> Self {
        Self(Substs {
            orbit_increment,
            ..self.0
        })
    }

    fn build(self) -> Substs<'a, D, P> {
        self.0
    }
}

pub struct Substs<'a, D: DistanceTy, P: PeriodTy> {
    sys_template: Option<&'a str>,
    gen_template: &'a str,
    num_of_bodies: i32,
    orbit_increment: OrbitIncrement<D, P>,
}

impl<D: DistanceTy, P: PeriodTy> Substs<'_, D, P> {
    pub fn generate_sys(&self) -> Result<()> {
        let orbital_subst_present = self.substs_present()?;

        let mut file = File::create("sysgen-output.sc")?;

        writeln!(
            file,
            "// Generated with {} v{}\n",
            env!("CARGO_PKG_NAME"),
            env!("CARGO_PKG_VERSION"),
        )
        .context("failed to append header")?;

        if let Some(sys_template) = self.sys_template {
            writeln!(file, "{sys_template}").context("failed to append system template")?;
        }

        let patterns = if let Some(orbital_subst_present) = orbital_subst_present.as_str() {
            vec!["<NAME>", "<I>", orbital_subst_present]
        } else {
            vec!["<NAME>", "<I>"]
        };

        let matcher = AhoCorasick::builder()
            .match_kind(MatchKind::LeftmostFirst)
            .build(patterns)
            .context("failed to build `AhoCorasick`")?;

        let orbit = match orbital_subst_present {
            OrbitalSubstPresent::No => None,
            OrbitalSubstPresent::Sma | OrbitalSubstPresent::SmaKm
                if let OrbitIncrement::Sma(start, inc) = self.orbit_increment =>
            {
                if matches!(orbital_subst_present, OrbitalSubstPresent::Sma) {
                    Some((start.as_au().as_f64(), inc.as_au().as_f64()))
                } else {
                    Some((start.as_km().as_f64(), inc.as_km().as_f64()))
                }
            }
            OrbitalSubstPresent::Per | OrbitalSubstPresent::PerDays
                if let OrbitIncrement::Per(start, inc) = self.orbit_increment =>
            {
                if matches!(orbital_subst_present, OrbitalSubstPresent::Per) {
                    Some((start.as_years().as_f64(), inc.as_years().as_f64()))
                } else {
                    Some((start.as_days().as_f64(), inc.as_days().as_f64()))
                }
            }
            _ => bail!(
                "substitutions present in `gen_template` and passed `orbit_increment` do not \
                 match!"
            ),
        };

        for i in 0..self.num_of_bodies {
            let replace_with = if let Some((start, inc)) = &orbit {
                let orbit = start + *inc * i as f64;

                vec![
                    format!("sysgen_outputted_body_{i}"),
                    i.to_string(),
                    f64::to_string(&orbit),
                ]
            } else {
                vec![format!("sysgen_outputted_body_{i}"), i.to_string()]
            };

            writeln!(file, "{}", self.apply_substs(&matcher, &replace_with, i)?)
                .context(format!("failed to append body with the index `{i}`"))?;
        }

        Ok(())
    }

    /// Basically a lint pass.
    fn check_for_bad_substs(&self) -> Result<()> {
        // Need to find out how to search for `<*>` without regex

        todo!();
    }

    fn substs_present(&self) -> Result<OrbitalSubstPresent> {
        let name = self.subst_present(Subst::Name)?;
        let index = self.subst_present(Subst::Index)?;

        if !name && !index {
            bail!("either the substitution `<NAME>` or `<I>` need to be present!");
        }

        let sma = self.subst_present(Subst::Sma)?;
        let sma_km = self.subst_present(Subst::SmaKm)?;
        let per = self.subst_present(Subst::Per)?;
        let per_days = self.subst_present(Subst::PerDays)?;

        let orbital_subst_present = match (sma, sma_km, per, per_days) {
            (false, false, false, false) => OrbitalSubstPresent::No,

            (true, false, false, false) => OrbitalSubstPresent::Sma,
            (false, true, false, false) => OrbitalSubstPresent::SmaKm,
            (false, false, true, false) => OrbitalSubstPresent::Per,
            (false, false, false, true) => OrbitalSubstPresent::PerDays,

            (_, _, _, _) => bail!(formatdoc! {
                "
                too many substitutions! Present substitutions:
                    `<SMA>`: `{sma}`,
                    `<SMA_KM>`: `{sma_km}`,
                    `<PER>`: `{per}`,
                    `<PER_DAYS>`: `{per_days}`
                "
            }),
        };

        Ok(orbital_subst_present)
    }

    fn subst_present(&self, subst: Subst) -> Result<bool> {
        let contains = |subst| match self.gen_template.matches(subst).count() {
            0usize => Ok(false),
            1usize => Ok(true),
            count @ 2usize.. => {
                bail!("too many occurences of substitution `{subst}`, count: `{count}`")
            }
        };

        contains(subst.as_str())
    }

    fn apply_substs(
        &self,
        matcher: &AhoCorasick,
        replace_with: &[impl AsRef<str>],
        i: i32,
    ) -> Result<String> {
        matcher
            .try_replace_all(self.gen_template, replace_with)
            .context("failed to apply substitutions using `AhoCorasick`")
    }
}

#[derive(Clone, Copy)]
enum Subst {
    Name,
    Index,
    Sma,
    SmaKm,
    Per,
    PerDays,
}

impl Subst {
    fn as_str(self) -> &'static str {
        match self {
            Subst::Name => "<NAME>",
            Subst::Index => "<I>",
            Subst::Sma => "<SMA>",
            Subst::SmaKm => "<SMA_KM>",
            Subst::Per => "<PER>",
            Subst::PerDays => "<PER_DAYS>",
        }
    }
}

#[derive(Clone, Copy)]
enum OrbitalSubstPresent {
    No,
    Sma,
    SmaKm,
    Per,
    PerDays,
}

impl OrbitalSubstPresent {
    fn as_str(self) -> Option<&'static str> {
        match self {
            OrbitalSubstPresent::No => None,
            OrbitalSubstPresent::Sma => Some("<SMA>"),
            OrbitalSubstPresent::SmaKm => Some("<SMA_KM>"),
            OrbitalSubstPresent::Per => Some("<PER>"),
            OrbitalSubstPresent::PerDays => Some("<PER_DAYS>"),
        }
    }
}

#[derive(Clone, Copy)]
pub enum OrbitIncrement<D: DistanceTy, P: PeriodTy> {
    No,
    /// Increment each new body's semi-major axis by a number. Will be converted
    /// between au and km automatically, depending on which substitution is
    /// present.
    Sma(Distance<D>, Distance<D>),
    /// Increment each new body's orbital period by a number. Will be converted
    /// between years and days automatically, depending on which substitution is
    /// present.
    Per(Period<P>, Period<P>),
}

#[derive(Clone, Copy)]
pub struct Distance<D: DistanceTy> {
    dist_data: f64,
    dist_marker: PhantomData<D>,
}

impl<D: DistanceTy> Distance<D> {
    pub fn as_km(self) -> Distance<Km> {
        D::__as_km_thunk()(self)
    }

    pub fn as_au(self) -> Distance<Au> {
        D::__as_au_thunk()(self)
    }

    pub fn new(dist: f64) -> Self {
        Self {
            dist_data: dist,
            dist_marker: PhantomData,
        }
    }

    pub fn as_f64(self) -> f64 {
        self.dist_data
    }
}

impl Distance<Km> {
    fn __as_km(self) -> Self {
        self
    }

    fn __as_au(self) -> Distance<Au> {
        Distance::<Au> {
            dist_data: self.dist_data * KM_TO_AU,
            dist_marker: PhantomData,
        }
    }
}

impl Distance<Au> {
    fn __as_km(self) -> Distance<Km> {
        Distance::<Km> {
            dist_data: self.dist_data * AU_TO_KM,
            dist_marker: PhantomData,
        }
    }

    fn __as_au(self) -> Self {
        self
    }
}

pub trait DistanceTy: Copy + sealed::Sealed {
    fn __as_km_thunk() -> fn(Distance<Self>) -> Distance<Km>;

    fn __as_au_thunk() -> fn(Distance<Self>) -> Distance<Au>;
}

impl DistanceTy for () {
    fn __as_km_thunk() -> fn(Distance<Self>) -> Distance<Km> {
        unimplemented!("`()` is used as a sentinel value");
    }

    fn __as_au_thunk() -> fn(Distance<Self>) -> Distance<Au> {
        unimplemented!("`()` is used as a sentinel value");
    }
}

#[derive(Clone, Copy)]
pub struct Km;

impl DistanceTy for Km {
    fn __as_km_thunk() -> fn(Distance<Self>) -> Distance<Self> {
        Distance::<Self>::__as_km
    }

    fn __as_au_thunk() -> fn(Distance<Self>) -> Distance<Au> {
        Distance::<Self>::__as_au
    }
}

#[derive(Clone, Copy)]
pub struct Au;

impl DistanceTy for Au {
    fn __as_km_thunk() -> fn(Distance<Self>) -> Distance<Km> {
        Distance::<Self>::__as_km
    }

    fn __as_au_thunk() -> fn(Distance<Self>) -> Distance<Self> {
        Distance::<Self>::__as_au
    }
}

#[derive(Clone, Copy)]
pub struct Period<P: PeriodTy> {
    per_data: f64,
    per_marker: PhantomData<P>,
}

impl<P: PeriodTy> Period<P> {
    pub fn as_days(self) -> Period<Days> {
        P::__as_days_thunk()(self)
    }

    pub fn as_years(self) -> Period<Years> {
        P::__as_years_thunk()(self)
    }

    pub fn new(per: f64) -> Self {
        Self {
            per_data: per,
            per_marker: PhantomData,
        }
    }

    pub fn as_f64(self) -> f64 {
        self.per_data
    }
}

impl Period<Days> {
    fn __as_days(self) -> Self {
        self
    }

    fn __as_years(self) -> Period<Years> {
        Period::<Years> {
            per_data: self.per_data * DAYS_TO_YEARS,
            per_marker: PhantomData,
        }
    }
}

impl Period<Years> {
    fn __as_days(self) -> Period<Days> {
        Period::<Days> {
            per_data: self.per_data * YEARS_TO_DAYS,
            per_marker: PhantomData,
        }
    }

    fn __as_years(self) -> Self {
        self
    }
}

pub trait PeriodTy: Copy + sealed::Sealed {
    fn __as_days_thunk() -> fn(Period<Self>) -> Period<Days>;

    fn __as_years_thunk() -> fn(Period<Self>) -> Period<Years>;
}

impl PeriodTy for () {
    fn __as_days_thunk() -> fn(Period<Self>) -> Period<Days> {
        unimplemented!("`()` is used as a sentinel value");
    }

    fn __as_years_thunk() -> fn(Period<Self>) -> Period<Years> {
        unimplemented!("`()` is used as a sentinel value");
    }
}

#[derive(Clone, Copy)]
pub struct Days;

impl PeriodTy for Days {
    fn __as_days_thunk() -> fn(Period<Self>) -> Period<Self> {
        Period::<Self>::__as_days
    }

    fn __as_years_thunk() -> fn(Period<Self>) -> Period<Years> {
        Period::<Self>::__as_years
    }
}

#[derive(Clone, Copy)]
pub struct Years;

impl PeriodTy for Years {
    fn __as_days_thunk() -> fn(Period<Self>) -> Period<Days> {
        Period::<Self>::__as_days
    }

    fn __as_years_thunk() -> fn(Period<Self>) -> Period<Self> {
        Period::<Self>::__as_years
    }
}

fn main() -> Result<()> {
    let substs = SubstsBuilder::<_, ()>::new(include_str!("../gen-template.txt"))
        .with_sys_template(include_str!("../sys-template.txt"))
        .with_num_of_bodies(20000i32)
        .with_orbit_increment(OrbitIncrement::Sma(
            Distance::<Km>::new(190000.0f64),
            Distance::<Km>::new(0.2f64),
        ))
        .build();

    substs.generate_sys().context("failed to generate system")?;

    Ok(())
}
