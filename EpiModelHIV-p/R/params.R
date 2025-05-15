
# MSM -----------------------------------------------------------------

#' @title Epidemic Model Parameters
#'
#' @description Sets the epidemic parameters for stochastic network models
#'              simulated with \code{\link{netsim}} for EpiModelHIV
#'
#' @param netstats Target statistics and related network initialization data
#'        from the standard ARTnet workflow.
#'
#' @param hiv.test.rate Mean probability of HIV testing per time step for
#'        Black/Hispanic/White MSM (vector of length 3).
#' @param hiv.test.late.prob Proportion of Black/Hispanic/White MSM who test
#'        only during AIDS stage infection (vector of length 3).
#' @param test.window.int Length of the HIV test window period in time steps.
#' @param tt.partial.supp.prob Proportion of Black/Hispanic/White MSM who enter
#'        partial viral suppression category after ART initiation (vector of
#'        length 3).
#' @param tt.full.supp.prob Proportion of Black/Hispanic/White MSM who enter full
#'        viral suppression category after ART initiation (vector of length 3).
#' @param tt.durable.supp.prob Proportion of Black/Hispanic/White MSM who enter
#'        durable viral suppression category after ART initiation (vector of
#'        length 3).
#'
#' @param tx.init.rate Probability per time step that a Black/Hispanic/White MSM
#'        who has tested positive will initiate treatment (vector of length 3).
#' @param tx.halt.partial.rate Probability per time step that
#'        Black/Hispanic/White MSM who have started treatment and assigned to
#'        the partial VL suppression category will stop treatment (vector of
#'        length 3).
#' @param tx.halt.full.rr Relative reduction in \code{tx.halt.partial.rate} for
#'        Black/Hispanic/White MSM in the full VL suppression category (vector
#'        of length 3).
#' @param tx.halt.durable.rr Relative reduction in \code{tx.halt.partial.rate}
#'        for Black/Hispanic/White MSM in the durable VL suppression category
#'        (vector of length 3).
#' @param tx.reinit.partial.rate Probability per time step that a
#'        Black/Hispanic/White MSM who has stopped treatment and assigned to the
#'        partial VL suppression category will restart treatment (vector of
#'        length 3).
#' @param tx.reinit.full.rr Relative reduction in \code{tx.reinit.partial.rate}
#'        for Black/Hispanic/White MSM in the full VL suppression category
#'        (vector of length 3).
#' @param tx.reinit.durable.rr Relative reduction in
#'        \code{tx.reinit.partial.rate} for Black/Hispanic/White MSM in the
#'        durable VL suppression category (vector of length 3).
#' @param max.time.off.tx.full.int Number of time steps off treatment for a full
#'        suppressor before onset of AIDS, including time before diagnosis.
#' @param max.time.on.tx.partial.int Number of time steps on treatment for a
#'        partial suppressor before onset of AIDS.
#' @param max.time.off.tx.partial.int Number of time steps off treatment for a
#'        partial suppressor before onset of AIDS, including time before
#'        diagnosis.
#' @param vl.acute.rise.int Number of time steps to peak viremia during acute
#'        infection.
#' @param vl.acute.peak Peak viral load (in log10 units) at the height of acute
#'        infection.
#' @param vl.acute.fall.int Number of time steps from peak viremia to set-point
#'        viral load during the acute infection period.
#' @param vl.set.point Set point viral load (in log10 units).
#' @param vl.aids.onset.int Number of time steps to AIDS for a treatment-naive
#'        patient.
#' @param vl.aids.int Duration of AIDS stage infection in time steps.
#' @param vl.aids.peak Maximum viral load during AIDS stage.
#' @param vl.full.supp Log10 viral load at full suppression on ART.
#' @param vl.part.supp Log10 viral load at partial suppression on ART.
#' @param vl.tx.down.rate Number of log10 units that viral load falls per time
#'        step from treatment initiation or re-initiation until the suppression
#'        level is reached (pre-AIDS stages).
#' @param vl.tx.aids.down.rate Number of log10 units that viral load falls per
#'        time step from treatment initiation or re-initiation until the
#'        suppression level is reached (AIDS stage).
#' @param vl.tx.up.rate Number of log10 units that viral load rises per time
#'        step from treatment halting until expected value.
#' @param aids.mort.rate Mortality rate of persons in the AIDS stage who are
#'        currently off ART per time step.
#'
#' @param a.rate Rate by time step at which MSM enter the population.
#' @param arrival.age Age (in years) of new arrivals.
#'
#' @param hiv.urai.prob Probability of infection for a man having unprotected
#'        receptive anal intercourse with an infected man at set point viral
#'        load.
#' @param hiv.uiai.prob Probability of infection for an uncircumcised man having
#'        unprotected insertive anal intercourse with an infected man at set
#'        point viral load.
#' @param hiv.trans.scale Relative scalar on base infection probabilities for model
#'        calibration for Black/Hispanic/White men (vector of length 3).
#' @param hiv.acute.rr Relative risk of infection (compared to that predicted by
#'        elevated viral load) when positive partner is in the acute stage.
#' @param hiv.circ.rr Relative risk of infection from insertive anal sex when the
#'        negative insertive partner is circumcised.
#'
#' @param hiv.cond.eff.rr Relative risk of HIV infection from anal sex when a condom is
#'        used properly (biological efficacy).
#' @param hiv.cond.fail.rr Relative risk of condom failure per time step for HIV for
#'        Black/Hispanic/White MSM, as a reduction in the cond.eff.rr parameter
#'        (vector of length 3).
#' @param hiv.circ.prob Probability that a Black/Hispanic/White new arrival in the
#'        population will be circumcised (vector of length 3).
#'
#' @param epistats GLMs for epidemiological parameter from the standard ARTnet
#'        workflow.
#' @param acts.aids.vl Viral load level after which sexual act rate goes to
#'        zero.
#' @param acts.scale Scalar for main/casual act rate for model calibration.
#' @param cond.scale Scalar for condom use probability for model calibration.
#'
#' @param riskh.start Time step at which behavioral risk history assessment
#'        occurs.
#' @param prep.start Time step at which the PrEP intervention should start.
#' @param prep.start.prob Probability of starting PrEP given current
#'        indications.
#' @param prep.adhr.dist Proportion of men who are low, medium, and high
#'        adherent to PrEP.
#' @param prep.adhr.rr Relative risk for infection per act associated with
#'        each level of adherence (from Grant).
#' @param prep.risk.reassess.int Interval for reassessment of risk
#'        indications of active PrEP users. Numerical value based on time unit
#'        conversion to yearly reassessment of PrEP risk. Default of 52 for
#'        yearly reassessment when time unit is weekly, 364 for yearly
#'        reassessment when time unit is daily, 12 for yearly reassessment when
#'        time unit is monthly, and Inf for no reassessment.
#' @param prep.require.lnt If \code{TRUE}, only start on PrEP if current time
#'        step is equal to the last negative test.
#' @param prep.discont.rate Rate of random discontinuation from PrEP per time step.
#' @param prep.tst.int Testing interval by time steps for those who are actively on PrEP. This
#'        overrides the mean testing interval parameters.
#' @param prep.risk.int Time window for assessment of risk eligibility for PrEP
#'        in time steps.
#'
#' @param rgc.prob Probability of rectal gonorrhea infection per act.
#' @param ugc.prob Probability of urethral gonorrhea infection per act.
#' @param vgc.prob Probability of vaginal gonorrhea infection per act.
#' @param rct.prob Probability of rectal chlamydia infection per act.
#' @param uct.prob Probability of urethral chlamydia infection per act.
#' @param vct.prob Probability of vaginal chlamydia infection per act.
#' @param rgc.sympt.prob Probability of symptoms given infection with rectal
#'        gonorrhea.
#' @param ugc.sympt.prob Probability of symptoms given infection with urethral
#'        gonorrhea.
#' @param vgc.sympt.prob Probability of symptoms given infection with vaginal
#'        gonorrhea.
#' @param rct.sympt.prob Probability of symptoms given infection with rectal
#'        chlamydia.
#' @param uct.sympt.prob Probability of symptoms given infection with urethral
#'        chlamydia.
#' @param vct.sympt.prob Probability of symptoms given infection with vaginal
#'        chlamydia.
#'
#' @param rgc.ntx.int Average duration in time steps of untreated rectal gonorrhea.
#' @param ugc.ntx.int Average duration in time steps of untreated urethral gonorrhea.
#' @param vgc.ntx.int Average duration in time steps of untreated vaginal gonorrhea.
#' @param gc.tx.int Average duration in time steps of treated gonorrhea (all sites).
#' @param rct.ntx.int Average duration in time steps of untreated rectal chlamydia.
#' @param uct.ntx.int Average duration in time steps of untreated urethral chlamydia.
#' @param uct.ntx.int Average duration in time steps of untreated vaginal chlamydia.
#' @param ct.tx.int Average duration in time steps of treated chlamydia (all sites).
#'
#' @param gc.sympt.tx.prob Probability of treatment for symptomatic gonorrhea
#'        for Black/Hispanic/White men and women (vector of length 6).
#' @param ct.sympt.tx.prob Probability of treatment for symptomatic chlamydia
#'        for Black/Hispanic/White men and women (vector of length 6).
#' @param gc.asympt.tx.prob Probability of treatment for asymptomatic gonorrhea
#'        for Black/Hispanic/White men and women (vector of length 6).
#' @param ct.asympt.tx.prob Probability of treatment for asymptomatic chlamydia
#'        for Black/Hispanic/White men and women (vector of length 6).
#'
#' @param prep.sti.screen.int Interval in time steps between STI screening at PrEP
#'        visits.
#' @param prep.sti.tx.prob Probability of treatment given positive screening
#'        during PrEP visit.
#' @param sti.cond.eff.rr Relative risk of STI infection from anal sex when a
#'        condom is used properly (biological efficacy).
#' @param sti.cond.fail.rr Relative risk of condom failure for STI for
#'        Black/Hispanic/White MSM, as a reduction in \code{sti.cond.eff.rr}
#'        (vector of length 3).
#' @param hiv.rgc.rr Relative risk of HIV infection given current rectal
#'        gonorrhea.
#' @param hiv.ugc.rr Relative risk of HIV infection given current urethral
#'        gonorrhea.
#' @param hiv.rct.rr Relative risk of HIV infection given current rectal
#'        chlamydia.
#' @param hiv.uct.rr Relative risk of HIV infection given current urethral
#'        chlamydia.
#' @param hiv.dual.rr Additive proportional risk, from 0 to 1, for HIV infection
#'        given dual infection with both gonorrhea and chlamydia.
#'
#' @param part.ident.start Time step to initiate the partner identification
#'        process. Default of \code{Inf} indicates no partner identification.
#' @param part.index.window.int Number of prior time steps for which a newly
#'        diagnosed index patient is eligible for partner notification. Default
#'        of 0 indicates index eligible only if newly diagnosed at current time
#'        step.
#' @param part.index.degree The minimum number of partnerships an index patient
#'        may have for partner elicitation. Default of \code{1} indicating all
#'        partnerships under consideration.
#' @param part.index.prob Probability that an index patient will initiate
#'        partner services.
#' @param part.ident.main.window.int The number of time steps in the past that a main
#'        partnership qualifies for partner identification.
#' @param part.ident.casl.window.int The number of time steps in the past that a
#'        casual partnership qualifies for partner identification.
#' @param part.ident.ooff.window.int The number of time steps in the past that a
#'        one-off partnership qualifies for partner identification.
#' @param part.ident.main.prob Probability that a main partner is identified
#'        through partner identification.
#' @param part.ident.casl.prob Probability that a casual partner is identified
#'        through partner identification.
#' @param part.ident.ooff.prob Probability that a one-time partner is identified
#'        through partner identification.
#' @param part.hiv.test.rate Mean probability per time step of HIV testing for a
#'        Black/Hispanic/White MSM who has been identified as the partner of an
#'        index HIV+ MSM.
#' @param part.prep.start.prob Probability of an individual identified through
#'        partner identification starting PrEP given current indications at the
#'        current time step.
#' @param part.tx.init.rate Probability that a Black/Hispanic/White MSM who has
#'        been identified as the partner of an incident HIV+ MSM will initiate
#'        treatment during the current time step.
#' @param part.tx.halt.rate Probability per time step that Black/Hispanic/White
#'        MSM who have have been identified through partner identification,
#'        started treatment and assigned to the partial VL suppression category
#'        will stop treatment (vector of length 3).
#' @param part.tx.reinit.rate Probability per time step that a
#'        Black/Hispanic/White MSM who has been identified through partner
#'        identification, stopped treatment will restart treatment (vector of
#'        length 3).
#' @param ... Additional arguments passed to the function.
#'
#' @section Modifying Time-Specific Parameters:
#' The default time steps of these parameters are weekly time steps.
#' Users must modify parameters ending in .rate, .int, and .start when using
#' non-weekly time steps. For example, when using daily time steps, .int
#' parameters should be multiplied by 7 and .rate parameters should be divided
#' by 7.
#'
#' @return
#' A list object of class \code{param_msm}, which can be passed to
#' EpiModel function \code{netsim}.
#'
#' @keywords msm
#'
#' @export
#'
param_msm <- function(netstats,
                      time.unit = 7,


                      # Clinical
                      hiv.test.rate = c(0.01325, 0.0125, 0.0124),
                      hiv.test.late.prob = c(0, 0, 0),
                      test.window.int = 3,
                      tt.partial.supp.prob = c(0, 0, 0),
                      tt.full.supp.prob = c(1, 1, 1),
                      tt.durable.supp.prob = c(0, 0, 0),
                      tx.init.rate = c(0.092, 0.092, 0.127),
                      tx.halt.partial.rate = c(0.0102, 0.0102, 0.0071),
                      tx.halt.full.rr = c(0.45, 0.45, 0.45),
                      tx.halt.durable.rr = c(0.45, 0.45, 0.45),
                      tx.reinit.partial.rate = c(0.00066, 0.00066, 0.00291),
                      tx.reinit.full.rr = c(1, 1, 1),
                      tx.reinit.durable.rr = c(1, 1, 1),

                      # HIV natural history
                      max.time.off.tx.full.int = 780,
                      max.time.on.tx.partial.int = 520,
                      max.time.off.tx.partial.int = 520,
                      vl.acute.rise.int = 3,
                      vl.acute.peak = 6.886,
                      vl.acute.fall.int = 3,
                      vl.set.point = 4.5,
                      vl.aids.onset.int = 520,
                      vl.aids.int = 104,
                      vl.aids.peak = 7,
                      vl.full.supp = 1.5,
                      vl.part.supp = 3.5,
                      vl.tx.down.rate = 0.25,
                      vl.tx.aids.down.rate = 0.25,
                      vl.tx.up.rate = 0.25,
                      aids.mort.rate = 0.009615,

                      # Demographic
                      a.rate = 0.00052,
                      arrival.age = 15,

                      # HIV infection prob
                      hiv.urai.prob = 0.008938,
                      hiv.uiai.prob = 0.003379,
                      hiv.trans.scale = c(1, 1, 1),
                      hiv.acute.rr = 6,
                      hiv.circ.rr = 0.4,
                      hiv.cond.eff.rr = 0.95,
                      hiv.cond.fail.rr = c(0.25, 0.25, 0.25),
                      hiv.circ.prob = c(0.874, 0.874, 0.918),

                      # Behavioral
                      epistats,
                      acts.aids.vl = 5.75,
                      acts.scale = 1,
                      cond.scale = 1,

                      # STI epi

                      #Transmission probabilties are the tuning parameters
                      rgc.prob = c(0.0,0.0,0.0),
                      ugc.prob = c(0.25,0.25,0.25),
                      vgc.prob = c(0.35,0.35,0.35),
                      rct.prob = c(0.0,0.0,0.0),
                      uct.prob = c(0.20,0.20,0.20),
                      vct.prob = c(0.30,0.30,0.30),

                      ##Symptomology not used
                      rgc.sympt.prob = 0.16,
                      ugc.sympt.prob = 0.80,
                      vgc.sympt.prob = 0.80,
                      rct.sympt.prob = 0.14,
                      uct.sympt.prob = 0.58,
                      vct.sympt.prob = 0.58,

                      ##Duration of treated and untreated infection drawn from the literature
                      rgc.ntx.int = 52,
                      ugc.ntx.int = 52,
                      vgc.ntx.int = 52,
                      gc.tx.int = 2,
                      rct.ntx.int = 70,
                      uct.ntx.int = 70,
                      vct.ntx.int = 70,
                      ct.tx.int = 2,

                      ##Testing is going to be determined by a regression model for NSFG data
                      gc.sympt.tx.prob = c(0.95, 0.95, 0.95,0.95, 0.95, 0.95),
                      ct.sympt.tx.prob = c(0.9, 0.9, 0.9,0.9, 0.9, 0.9),
                      gc.asympt.tx.prob = c(0.15, 0.15, 0.15,0.15, 0.15, 0.15),
                      ct.asympt.tx.prob = c(0.15, 0.15, 0.15,0.15, 0.15, 0.15),
                      sti.cond.eff.rr = 0.9,
                      sti.cond.fail.rr = c(0.20, 0.20, 0.20),
                      hiv.rgc.rr = 2.78,
                      hiv.ugc.rr = 1.73,
                      hiv.rct.rr = 2.78,
                      hiv.uct.rr = 1.73,
                      hiv.dual.rr = 0.2,

                      # PrEP
                      riskh.start = Inf,
                      prep.start = Inf,
                      prep.start.prob = c(0.2, 0.2, 0.2),
                      prep.adhr.dist = c(0.089, 0.127, 0.784),
                      prep.adhr.rr = c(0.69, 0.19, 0.01),
                      prep.discont.rate = rep(1 - (2 ^ (-1 / (224.4237 / 7))), 3),
                      prep.tst.int = 12.8571,
                      prep.risk.int = 26,
                      prep.sti.screen.int = 26,
                      prep.sti.tx.prob = 1,
                      prep.risk.reassess.int = 52,
                      prep.require.lnt = TRUE,

                      # Partner notification
                      part.ident.start = Inf,
                      part.index.window.int = 0,
                      part.index.degree = 1,
                      part.index.prob = 1,
                      part.ident.main.window.int = 12,
                      part.ident.casl.window.int = 12,
                      part.ident.ooff.window.int = 12,
                      part.ident.main.prob = 1,
                      part.ident.casl.prob = 1,
                      part.ident.ooff.prob = 1,
                      part.hiv.test.rate = c(1, 1, 1),
                      part.prep.start.prob = c(0.5, 0.5, 0.5),
                      part.tx.init.rate = c(0.6, 0.6, 0.8),
                      part.tx.halt.rate = c(0.00102, 0.00102, 0.00071),
                      part.tx.reinit.rate = c(0.5, 0.5, 0.5),
                      ...) {

  p <- get_args(formal.args = formals(sys.function()),
                dot.args = list(...))

  class(p) <- "param.net"
  return(p)
}


#' @title Epidemic Model Initial Conditions
#'
#' @description Sets the initial conditions for a stochastic epidemic models
#'              simulated with \code{\link{netsim}}.
#'
#' @param prev.ugc Initial prevalence of urethral gonorrhea.
#' @param prev.rgc Initial prevalence of rectal gonorrhea.
#' @param prev.vgc Initial prevalence of vaginal gonorrhea.
#' @param prev.uct Initial prevalence of urethral chlamydia.
#' @param prev.rct Initial prevalence of rectal chlamydia.
#' @param prev.vct Initial prevalence of vaginal chlamydia.
#' @param ... Additional arguments passed to function.
#'
#' @return
#' A list object of class \code{init_msm}, which can be passed to EpiModel
#' function \code{\link{netsim}}.
#'
#' @keywords msm
#'
#' @export
init_msm <- function(prev.ugc = 0,
                     prev.rgc = 0,
                     prev.vgc = 0,
                     prev.uct = 0,
                     prev.rct = 0,
                     prev.vct = 0,
                     ...) {

  p <- get_args(formal.args = formals(sys.function()),
                dot.args = list(...))

  class(p) <- "init.net"
  return(p)
}


#' @title Epidemic Model Control Settings
#'
#' @description Sets the controls for stochastic network models simulated with
#'              \code{\link{netsim}}.
#'
#' @param simno Unique ID for the simulation run, used for file naming purposes
#'        if used in conjunction with the \code{EpiModelHPC} package.
#' @param nsims Number of simulations.
#' @param ncores Number of cores per run, if parallel processing is used.
#' @param nsteps Number of time steps per simulation.
#' @param start Starting time step for simulation, with default to 1 to run new
#'        simulation. This may also be set to 1 greater than the final time
#'        step of a previous simulation to resume the simulation with different
#'        parameters.
#' @param cumulative.edgelist If \code{TRUE}, calculates a cumulative edgelist
#'        within the network simulation module. This is used when tergmLite is
#'        used and the entire networkDynamic object is not used.
#' @param truncate.el.cuml Number of time steps of the cumulative edgelist to
#'        retain. See help file for \code{EpiModel::update_cumulative_edgelist}
#'        for options.
#' @param initialize.FUN Module function to use for initialization of the
#'        epidemic model.
#' @param aging.FUN Module function for aging.
#' @param departure.FUN Module function for general and disease-related
#'        departures.
#' @param arrival.FUN Module function for entries into the sexually active
#'        population.
#' @param partident.FUN Module function for partner identification process.
#' @param hivtest.FUN Module function for HIV diagnostic disease testing.
#' @param hivtx.FUN Module function for ART initiation and adherence.
#' @param prep.FUN Module function for PrEP initiation and utilization.
#' @param hivprogress.FUN Module function for HIV disease progression.
#' @param hivvl.FUN Module function for HIV viral load evolution.
#' @param resim_nets.FUN Module function for network resimulation at each time
#'        step.
#' @param acts.FUN Module function to simulate the number of sexual acts within
#'        partnerships.
#' @param condoms.FUN Module function to simulate condom use within acts.
#' @param position.FUN Module function to simulate sexual position within acts.
#' @param hivtrans.FUN Module function to stochastically simulate HIV
#'        transmission over acts given individual and dyadic attributes.
#' @param stitrans.FUN Module function to simulate GC/CT transmission over
#'        current edgelist.
#' @param stirecov.FUN Module function to simulate recovery from GC/CT,
#'        heterogeneous by disease, site, symptoms, and treatment status.
#' @param stitx.FUN Module function to simulate treatment of GC/CT.
#' @param prev.FUN Module function to calculate prevalence summary statistics.
#' @param verbose.FUN Module function to print model progress to the console or
#'        external text files.
#' @param save.nwstats Calculate and save network statistics as defined in the
#'        \code{simnet} modules.
#' @param nwstats.formula.1 Right-hand side formula for network statistics to
#'        monitor for network 1, with default \code{"formation"} equal to the
#'        model formula.
#' @param nwstats.formula.2 Right-hand side formula for network statistics to
#'        monitor for network 2, with default \code{"formation"} equal to the
#'        model formula.
#' @param nwstats.formula.3 Right-hand side formula for network statistics to
#'        monitor for network 3, with default \code{"formation"} equal to the
#'        model formula.
#' @param tergmLite Logical indicating usage of \code{tergmLite} (non-modifiable
#'        currently for \code{EpiModelHIV}).
#' @param tergmLite.track.duration If \code{TRUE}, track duration information
#'        for models in \code{tergmLite} simulations.
#' @param verbose If \code{TRUE}, print out simulation progress to the console
#'        if in interactive mode or text files if in batch mode.
#' @param set.control.ergm Control arguments passed to \code{ergm}'s
#'        \code{simulate_formula.network}.
#' @param set.control.tergm Control arguments passed to \code{tergm}'s
#'        \code{simulate_formula.network}.
#' @param set.control.stergm Deprecated; use \code{set.control.tergm} instead.
#' @param ... Additional arguments passed to the function.
#'
#' @return
#' A list object of class \code{control_msm}, which can be passed to the
#' EpiModel function \code{netsim}.
#'
#' @keywords msm
#'
#' @export
control_msm <- function(simno = 1,
                        nsims = 1,
                        ncores = 1,
                        nsteps = 100,
                        start = 1,
                        cumulative.edgelist = TRUE,
                        truncate.el.cuml = 3,
                        initialize.FUN = initialize_msm,
                        aging.FUN = aging_msm,
                        departure.FUN = departure_msm,
                        arrival.FUN = arrival_msm,
                        partident.FUN = partident_msm,
                        #hivtest.FUN = hivtest_msm,
                        #hivtx.FUN = hivtx_msm,
                        #hivprogress.FUN = hivprogress_msm,
                        #hivvl.FUN = hivvl_msm,
                        resim_nets.FUN = simnet_msm,
                        acts.FUN = acts_msm,
                        condoms.FUN = condoms_msm,
                        position.FUN = position_msm,
                        #prep.FUN = prep_msm,
                        #hivtrans.FUN = hivtrans_msm,
                        stitrans.FUN = stitrans_msm,
                        stirecov.FUN = stirecov_msm,
                        stitx.FUN = stitx_msm,
                        prev.FUN = prevalence_msm,
                        verbose.FUN = verbose.net,
                        save.nwstats = FALSE,
                        save.nwstats2 = TRUE,
                        nwstats.formula.1 = "formation",
                        nwstats.formula.2 = "formation",
                        nwstats.formula.3 = "formation",
                        # nwstats.formula.1 = ~edges +
                        #   nodematch("age.grp", diff = TRUE) +
                        #   nodefactor("age.grp", levels = c(-3,-4)) +
                        #   absdiff(~age + 2.0*(sex == 2)) +
                        #   nodematch("race", diff = TRUE) +
                        #   nodefactor("race", levels = -2) +
                        #   nodefactor("deg.casl.c.het", levels = -1) +
                        #   concurrent +
                        #   degrange(from = 3) +
                        #   offset(nodematch("sex", diff = FALSE)),
                        # nwstats.formula.2 = ~edges +
                        #   nodematch("age.grp", diff = TRUE) +
                        #   nodefactor("age.grp", levels = -4) +
                        #   absdiff(~age + 2.0*(sex == 2)) +
                        #   nodefactor("deg.main.c.het", levels = -1) +
                        #   concurrent +
                        #   degrange(from = 4) +
                        #   #If race = TRUE:
                        #   nodematch("race", diff = TRUE) +
                        #   nodefactor("race", levels = -1) +
                        #   nodefactor("age15", levels = -1) +
                        #   offset(nodematch("sex", diff = FALSE)),
                        # nwstats.formula.3 = ~edges +
                        #   nodematch("age.grp", diff = FALSE) +
                        #   nodefactor("age.grp", levels = -4) +
                        #   absdiff(~age + 2.0*(sex == 2)) +
                        #   nodefactor("risk.grp", levels = 5) +
                        #   nodefactor("deg.tot.c.het", levels = -1) +
                        #   nodematch("race", diff = TRUE) +
                        #   nodefactor("race", levels = -3) +
                        #   offset(nodematch("sex", diff = FALSE)),
                        tergmLite = TRUE,
                        tergmLite.track.duration = FALSE,
                        #set.control.ergm = control.simulate.formula(MCMC.prop = ~strat(attr = ~paste(age.grp, race, het), empirical = TRUE) + sparse, MCMC.burnin = 1e+05),
                        set.control.ergm = control.simulate.formula(MCMC.burnin = 2e5),
                        set.control.stergm = NULL,
                        #set.control.tergm = control.simulate.formula.tergm(MCMC.prop = ~strat(attr = ~paste(age.grp, race, het), empirical = TRUE) + discord + sparse, MCMC.burnin.max = 1e+06),
                        set.control.tergm = control.simulate.formula.tergm(),
                        verbose = TRUE,
                        ...) {
  if (!missing(set.control.stergm)) {
    warning("set.control.stergm is deprecated and will be removed in a future
             version; use set.control.tergm instead.")
  }

  formal.args <- formals(sys.function())
  dot.args <- list(...)
  p <- get_args(formal.args, dot.args)

  p$skip.check <- TRUE
  p$save.transmat <- FALSE

  bi.mods <- grep(".FUN", names(formal.args), value = TRUE)
  bi.mods <- bi.mods[which(sapply(bi.mods, function(x) !is.null(eval(parse(text = x))),
                                  USE.NAMES = FALSE) == TRUE)]
  p$bi.mods <- bi.mods
  p$user.mods <- grep(".FUN", names(dot.args), value = TRUE)
  p[["f.names"]] <- c(p[["bi.mods"]], p[["user.mods"]])
  p$save.other <- c("attr", "temp", "el")

  p$save.network <- FALSE
  if (is.null(p$verbose.int)) {
    p$verbose.int <- 1
  }

  p <- set.control.class("control.net", p)
  return(p)
}
