# Gallery snippets for the time-series statistics: pv_decompose,
# pv_forecast, and pv_changepoints. None of these is a new chart type -
# each one computes in R (base and stats only) and draws through
# machinery the package already had. Same format as gallery-core.R;
# data-raw/build-gallery.R assembles them into docs/.

## decompose
# explain: pv_decompose() splits one seasonal series into the three parts
#   it is made of and shows them as four aligned line panels: the
#   observed series, the slow trend, the repeating seasonal shape, and
#   the remainder neither explains. The statistics are stats::stl() -
#   seasonal-trend decomposition by loess, computed in R, with
#   method = "classical" swapping in stats::decompose() - and the panels
#   follow the decompose convention: a shared x axis so features line up
#   vertically, but each panel on its own y scale, so compare timing
#   across panels and read amplitudes off each panel's own axis. Six
#   years of Swiss electricity production come apart cleanly here. The
#   seasonal panel is the Alps' water schedule: every July, snowmelt
#   pushes river and storage hydro about 1,200 GWh above the trend, and
#   every February the same schedule takes about 700 GWh away. The trend
#   underneath tells a different story - it sags through 2021 and 2022,
#   climbs to its peak in spring 2024 (the record production year), then
#   eases back through 2025. And the remainder keeps what neither part
#   explains: its biggest surprise, about +1,300 GWh in July 2024, was a
#   hydro month stronger than even the season allows for.
{
  monthly <- aggregate(gwh ~ date, pv_electricity, sum)
  pv_decompose(monthly, x = "date", y = "gwh", ylab = "GWh",
               title = "Switzerland's electricity, taken apart",
               subtitle = "Monthly production split into trend, seasonal shape, and remainder, 2020–2025",
               source = "Source: Bundesamt für Energie")
}

## forecast
# explain: pv_forecast() fits a time-series model in R - here
#   stats::HoltWinters(), nothing beyond base and stats - and extends the
#   line with the classic fan: nested 50/80/95% bands, widest outermost,
#   the point forecast continuing as a dashed line (dashes carry their
#   reserved projection meaning throughout polyviz), and a thin rule
#   where the observed data ends. The crosshair works over the projected
#   region too, reading the estimate and every interval. Be clear about
#   what the fan claims: the intervals are model-based - roughly, "if
#   the future behaves like the fitted past, the band should contain it
#   about that often" - and they know nothing about breaks ahead. This
#   series is its own proof: a fan drawn in 2019 would never have
#   contained 2020, when the pandemic took Swiss hotel nights from a
#   record 39.6 million down to 23.7. That crash now sits inside the
#   fitting window, which is why these bands are honestly wide - the
#   central path carries the recovery on past 45 million nights, but the
#   95% band four years out already spans roughly 30 to 69 million.
#   method = "arima" runs a small AIC search, method = "naive" is the
#   last-value baseline every fancier forecast has to beat, and the
#   fitted model is always recorded in the chart's payload.
{
  nights <- aggregate(nights ~ year, pv_tourism, sum)
  pv_line(nights, x = "year", y = "nights", ylab = "Hotel nights",
          xlab = NA,
          title = "Will the tourism record hold?",
          subtitle = "Hotel nights in Switzerland per year, with a four-year forecast fan",
          source = "Source: Bundesamt für Statistik – HESTA") |>
    pv_forecast(horizon = 4)
}

## changepoints
# explain: pv_changepoints() scans a single series for points where its
#   mean level shifts and stays shifted - binary segmentation with a BIC
#   stopping rule, computed from first principles in base R - and marks
#   each one through the same annotation machinery pv_vline() drives by
#   hand; levels = TRUE adds each stretch's mean as a dashed horizontal
#   line. It is a screening rule, not proof, and it finds sustained
#   shifts in level, not every wiggle: a smooth trend or a seasonal
#   cycle can be carved into spurious "shifts", so detrend or
#   deseasonalise first when a series has those (the decomposition above
#   is the tool). Here the rule earns its keep. Swiss monthly nuclear
#   production drops every summer, when reactors go down for refuelling
#   and maintenance, and in every earlier year the level found its way
#   back to the winter plateau above 2,000 GWh - even after 2021's long
#   overhaul, which lasted into December but ended. The one cut that
#   survives the BIC rule stands at May 2025: from there the series
#   never returns, holding between roughly 1,100 and 1,460 GWh to the
#   end of the data, and the fitted means say the level fell from about
#   1,860 GWh a month to about 1,270. The mark says where the level
#   changed, never why - and when no cut survives the rule, the chart
#   comes back unchanged with a message saying so.
{
  nuclear <- subset(pv_electricity, source == "Nuclear")
  pv_line(nuclear, x = "date", y = "gwh", ylab = "GWh", xlab = NA,
          title = "The summer nuclear output didn't come back",
          subtitle = "Monthly nuclear production, 2020–2025, with detected level shifts",
          source = "Source: Bundesamt für Energie") |>
    pv_changepoints(levels = TRUE)
}
