# Fit diagnostics of the C17 networks



####   MAIN PARTNERSHIP HET NETWORK
load("~/CAMP-HET-STI/est/fit_main.het.2011.rda")

# Static diagnostics on the ERGM fit
startclock <- proc.time()
    dx.main.het.2011.static <- netdx(fit_main.het.2011, nsims = 500, dynamic = FALSE,
                                keep.tedgelist = FALSE,
                                sequential = TRUE,
                                verbose = TRUE,
                                set.control.ergm = control.simulate.formula(MCMC.prop = ~strat(attr = ~paste(age.grp, race), empirical = TRUE) + sparse,
                                                        MCMC.burnin=5e6,
                                                        MCMC.interval=5e5))

    hours <- (proc.time()-startclock)['elapsed']/60/60
    hours
    dx.main.het.2011.static
    plot(dx.main.het.2011.static)

    save(dx.main.het.2011.static,file = "~/CAMP-HET-STI/DX/dx.main.het.2011.static.rda")

    tiff(filename = "~/CAMP-HET-STI/DX/dx.main.het.2011.static.tiff", height = 10.5, width = 8, units = "in", res = 250)
    par(mfrow = c(1, 1), mar = c(4,3,2.5,1), mgp = c(2,.5,0), cex=.7)
    plot(dx.main.het.2011.static)
    dev.off()



 # Dynamic diagnostics on the ERGM fit
    startclock <- proc.time()
    dx.main.het.2011.dynamic <- netdx(fit_main.het.2011, nsims = 5, nsteps = 8000, sequential = TRUE,
                                nwstats.formula = "formation",
                                keep.tedgelist = FALSE,
                                verbose = TRUE,
                                set.control.ergm = list(MCMC.burnin=3e6,
                                                        MCMC.interval=3e5,
                                                        parallel = 10),
                                set.control.tergm = list(MCMC.prop = ~strat(attr = ~paste(age.grp, race), empirical = TRUE) + discord + sparse,
                                                          MCMC.maxchanges = 1e8,
                                                          MCMC.burnin.min =3e5,
                                                          MCMC.burnin.max =3e5))

    hours <- (proc.time()-startclock)['elapsed']/60/60
    hours
    dx.main.het.2011.dynamic
    plot(dx.main.het.2011.dynamic)
    save(dx.main.het.2011.dynamic,file = "~/CAMP-HET-STI/DX/dx.main.het.2011.dynamic.rda")

    tiff(filename = "~/CAMP-HET-STI/DX/dx.main.het.2011.dynamic.tiff", height = 10.5, width = 8, units = "in", res = 250)
    par(mfrow = c(1, 1), mar = c(4,3,2.5,1), mgp = c(2,.5,0), cex=.7)
    plot(dx.main.het.2011.dynamic)
    dev.off()



####   CASUAL PARTNESHIPS HET NETWORK
    load("~/CAMP-HET-STI/est/fit_casl.het.2011.rda")

    # Static diagnostics on the ERGM fit
    startclock <- proc.time()
    dx.casl.het.2011.static <- netdx(fit_casl.het.2011, nsims = 500, dynamic = FALSE,
                                keep.tedgelist = FALSE,
                                sequential = TRUE,
                                verbose = TRUE,
                                set.control.ergm = control.simulate.formula(MCMC.prop = ~strat(attr = ~paste(age.grp, race), empirical = TRUE) + sparse,
                                                                             MCMC.burnin=3e6,
                                                                             MCMC.interval=3e5))

    hours <- (proc.time()-startclock)['elapsed']/60/60
    hours
    dx.casl.het.2011.static

    save(dx.casl.het.2011.static,file = "~/CAMP-HET-STI/DX/dx.casl.het.2011.static.rda")

    tiff(filename = "~/CAMP-HET-STI/DX/dx.casl.het.2011.static.tiff", height = 10.5, width = 8, units = "in", res = 250)
    par(mfrow = c(1, 1), mar = c(4,3,2.5,1), mgp = c(2,.5,0), cex=.7)
    plot(dx.casl.het.2011.static)
    dev.off()



    # Dynamic diagnostics on the ERGM fit
    startclock <- proc.time()
    dx.casl.het.2011.dynamic <- netdx(fit_casl.het.2011, nsims = 5, nsteps = 8000, sequential = TRUE,
                                nwstats.formula = "formation",
                                keep.tedgelist = FALSE,
                                verbose = TRUE,
                                set.control.ergm = list(MCMC.burnin=2e6,
                                                        MCMC.interval=2e6),
                                set.control.tergm = list(MCMC.prop = ~strat(attr = ~paste(age.grp, race), empirical = TRUE) + discord + sparse,
                                                          MCMC.maxchanges = 1e8,
                                                          MCMC.burnin.min =3e5,
                                                          MCMC.burnin.max =3e5))




    hours <- (proc.time()-startclock)['elapsed']/60/60
    hours
    dx.casl.het.2011.dynamic

    save(dx.casl.het.2011.dynamic,file = "~/CAMP-HET-STI/DX/dx.casl.het.2011.dynamic.rda")

    tiff(filename = "~/CAMP-HET-STI/DX/dx.casl.het.2011.dynamic.tiff", height = 10.5, width = 8, units = "in", res = 250)
    par(mfrow = c(1, 1), mar = c(4,3,2.5,1), mgp = c(2,.5,0), cex=.7)
    plot(dx.casl.het.2011.dynamic)
    dev.off()



####   ONE TIME PARTNESHIPS HET NETWORK


    load("~/CAMP-HET-STI/est/fit_inst.het.2011.rda")

    # Static diagnostics on the ERGM fit
    startclock <- proc.time()
    dx.inst.het.2011.static <- netdx(fit_inst.het.2011, nsims = 500, dynamic = FALSE,
                                keep.tedgelist = FALSE,
                                sequential = TRUE,
                                verbose = TRUE,
                                set.control.ergm =  control.simulate.formula(MCMC.prop = ~strat(attr = ~paste(age.grp, race), empirical = TRUE) + sparse,
                                                        MCMC.burnin=3e6,
                                                        MCMC.interval=3e5))

    hours <- (proc.time()-startclock)['elapsed']/60/60
    hours
    dx.inst.het.2011.static

    save(dx.inst.het.2011.static,file = "~/CAMP-HET-STI/DX/dx.inst.het.2011.static.rda")
    tiff(filename = "~/CAMP-HET-STI/DX/dx.inst.het.2011.static.tiff", height = 10.5, width = 8, units = "in", res = 250)
    par(mfrow = c(1, 1), mar = c(4,3,2.5,1), mgp = c(2,.5,0), cex=.7)
    plot(dx.inst.het.2011.static)
    dev.off()


   ##2019 networks
    ####   MAIN PARTNERSHIP HET NETWORK
    load("~/CAMP-HET-STI/est/fit_main.het.2019.rda")

    # Static diagnostics on the ERGM fit
    startclock <- proc.time()
    dx.main.het.2019.static <- netdx(fit_main.het.2019, nsims = 500, dynamic = FALSE,
                                      keep.tedgelist = FALSE,
                                      sequential = TRUE,
                                      verbose = TRUE,
                                      set.control.ergm = control.simulate.formula(MCMC.prop = ~strat(attr = ~paste(age.grp, race), empirical = TRUE) + sparse,
                                                                                  MCMC.burnin=3.2e6,
                                                                                  MCMC.interval=4e5))

    hours <- (proc.time()-startclock)['elapsed']/60/60
    hours
    dx.main.het.2019.static
    plot(dx.main.het.2019.static)

    save(dx.main.het.2019.static,file = "~/CAMP-HET-STI/DX/dx.main.het.2019.static.rda")

    tiff(filename = "~/CAMP-HET-STI/DX/dx.main.het.2019.static.tiff", height = 10.5, width = 8, units = "in", res = 250)
    par(mfrow = c(1, 1), mar = c(4,3,2.5,1), mgp = c(2,.5,0), cex=.7)
    plot(dx.main.het.2019.static)
    dev.off()



    # Dynamic diagnostics on the ERGM fit
    startclock <- proc.time()
    dx.main.het.2019.dynamic <- netdx(fit_main.het.2019, nsims = 5, nsteps = 8000, sequential = TRUE,
                                       nwstats.formula = "formation",
                                       keep.tedgelist = FALSE,
                                       verbose = TRUE,
                                       set.control.ergm = list(MCMC.burnin=3e6,
                                                               MCMC.interval=3e5,
                                                               parallel = 10),
                                       set.control.tergm = list(MCMC.prop = ~strat(attr = ~paste(age.grp, race), empirical = TRUE) + discord + sparse,
                                                                MCMC.maxchanges = 1e8,
                                                                MCMC.burnin.min =4e5,
                                                                MCMC.burnin.max =4e5))

    hours <- (proc.time()-startclock)['elapsed']/60/60
    hours
    dx.main.het.2019.dynamic
    plot(dx.main.het.2019.dynamic)
    save(dx.main.het.2019.dynamic,file = "~/CAMP-HET-STI/DX/dx.main.het.2019.dynamic.rda")

    tiff(filename = "~/CAMP-HET-STI/DX/dx.main.het.2019.dynamic.tiff", height = 10.5, width = 8, units = "in", res = 250)
    par(mfrow = c(1, 1), mar = c(4,3,2.5,1), mgp = c(2,.5,0), cex=.7)
    plot(dx.main.het.2019.dynamic)
    dev.off()



    ####   CASUAL PARTNESHIPS HET NETWORK
    load("~/CAMP-HET-STI/est/fit_casl.het.2019.rda")

    # Static diagnostics on the ERGM fit
    startclock <- proc.time()
    dx.casl.het.2019.static <- netdx(fit_casl.het.2019, nsims = 500, dynamic = FALSE,
                                      keep.tedgelist = FALSE,
                                      sequential = TRUE,
                                      verbose = TRUE,
                                      set.control.ergm = control.simulate.formula(MCMC.prop = ~strat(attr = ~paste(age.grp, race), empirical = TRUE) + sparse,
                                                                                  MCMC.burnin=3e6,
                                                                                  MCMC.interval=3e5))

    hours <- (proc.time()-startclock)['elapsed']/60/60
    hours
    dx.casl.het.2019.static

    save(dx.casl.het.2019.static,file = "~/CAMP-HET-STI/DX/dx.casl.het.2019.static.rda")

    tiff(filename = "~/CAMP-HET-STI/DX/dx.casl.het.2019.static.tiff", height = 10.5, width = 8, units = "in", res = 250)
    par(mfrow = c(1, 1), mar = c(4,3,2.5,1), mgp = c(2,.5,0), cex=.7)
    plot(dx.casl.het.2019.static)
    dev.off()



    # Dynamic diagnostics on the ERGM fit
    startclock <- proc.time()
    dx.casl.het.2019.dynamic <- netdx(fit_casl.het.2019, nsims = 5, nsteps = 8000, sequential = TRUE,
                                       nwstats.formula = "formation",
                                       keep.tedgelist = FALSE,
                                       verbose = TRUE,
                                       set.control.ergm = list(MCMC.burnin=2e6,
                                                               MCMC.interval=2e6),
                                       set.control.tergm = list(MCMC.prop = ~strat(attr = ~paste(age.grp, race), empirical = TRUE) + discord + sparse,
                                                                MCMC.maxchanges = 1e8,
                                                                MCMC.burnin.min =2e5,
                                                                MCMC.burnin.max =2e5))




    hours <- (proc.time()-startclock)['elapsed']/60/60
    hours
    dx.casl.het.2019.dynamic

    save(dx.casl.het.2019.dynamic,file = "~/CAMP-HET-STI/DX/dx.casl.het.2019.dynamic.rda")

    tiff(filename = "~/CAMP-HET-STI/DX/dx.casl.het.2019.dynamic.tiff", height = 10.5, width = 8, units = "in", res = 250)
    par(mfrow = c(1, 1), mar = c(4,3,2.5,1), mgp = c(2,.5,0), cex=.7)
    plot(dx.casl.het.2019.dynamic)
    dev.off()



    ####   ONE TIME PARTNESHIPS HET NETWORK


    load("~/CAMP-HET-STI/est/fit_inst.het.2019.rda")

    # Static diagnostics on the ERGM fit
    startclock <- proc.time()
    dx.inst.het.2019.static <- netdx(fit_inst.het.2019, nsims = 500, dynamic = FALSE,
                                      keep.tedgelist = FALSE,
                                      sequential = TRUE,
                                      verbose = TRUE,
                                      set.control.ergm =  control.simulate.formula(MCMC.prop = ~strat(attr = ~paste(age.grp, race), empirical = TRUE) + sparse,
                                                                                   MCMC.burnin=3e6,
                                                                                   MCMC.interval=3e5))

    hours <- (proc.time()-startclock)['elapsed']/60/60
    hours
    dx.inst.het.2019.static

    save(dx.inst.het.2019.static,file = "~/CAMP-HET-STI/DX/dx.inst.het.2019.static.rda")
    tiff(filename = "~/CAMP-HET-STI/DX/dx.inst.het.2019.static.tiff", height = 10.5, width = 8, units = "in", res = 250)
    par(mfrow = c(1, 1), mar = c(4,3,2.5,1), mgp = c(2,.5,0), cex=.7)
    plot(dx.inst.het.2019.static)
    dev.off()

