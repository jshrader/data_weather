"""Download global ERA5 monthly means for the encroachment project.

This is the global/monthly counterpart to the older hourly downloader in
`ERA1_download_era.py`. It targets the minimum variables needed for the
handed-off 1-degree weather controls:

- 2m_temperature
- total_precipitation

The script writes one NetCDF file per variable so the downstream R pipeline
can read each variable directly with `terra::rast()`.
"""

from __future__ import annotations

import argparse
from pathlib import Path
from typing import Iterable

import cdsapi


DEFAULT_DATASET = "reanalysis-era5-single-levels-monthly-means"
DEFAULT_VARIABLES = ("2m_temperature", "total_precipitation")
DEFAULT_OUTPUT_DIR = "/media/jgs/datadrive/data/weather/ecmwf/era5_monthly_means_world"


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--dataset", default=DEFAULT_DATASET)
    parser.add_argument("--start-year", type=int, default=1992)
    parser.add_argument("--end-year", type=int, default=2020)
    parser.add_argument(
        "--variable",
        dest="variables",
        action="append",
        choices=DEFAULT_VARIABLES,
        help="Repeat to request a subset of supported variables.",
    )
    parser.add_argument("--output-dir", default=DEFAULT_OUTPUT_DIR)
    parser.add_argument("--force", action="store_true")
    return parser.parse_args()


def build_years(start_year: int, end_year: int) -> list[str]:
    if end_year < start_year:
        raise ValueError("--end-year must be >= --start-year")
    return [str(year) for year in range(start_year, end_year + 1)]


def build_months() -> list[str]:
    return [f"{month:02d}" for month in range(1, 13)]


def target_path(output_dir: Path, variable: str, start_year: int, end_year: int) -> Path:
    filename = f"era5_monthly_means_{variable}_{start_year}_{end_year}.nc"
    return output_dir / filename


def candidate_requests(variable: str, years: Iterable[str], months: Iterable[str]) -> list[dict]:
    base = {
        "variable": [variable],
        "year": list(years),
        "month": list(months),
        "time": ["00:00"],
    }
    return [
        {
            **base,
            "product_type": ["monthly_averaged_reanalysis"],
            "data_format": "netcdf",
            "download_format": "unarchived",
        },
        {
            **base,
            "product_type": "monthly_averaged_reanalysis",
            "data_format": "netcdf",
            "download_format": "unarchived",
        },
        {
            **base,
            "product_type": "monthly_averaged_reanalysis",
            "format": "netcdf",
        },
        {
            **base,
            "product_type": ["monthly_averaged_reanalysis"],
            "format": "netcdf",
        },
    ]


def retrieve_with_fallback(
    client: cdsapi.Client,
    dataset: str,
    variable: str,
    years: list[str],
    months: list[str],
    destination: Path,
) -> None:
    last_error: Exception | None = None
    for index, request in enumerate(candidate_requests(variable, years, months), start=1):
        try:
            print(f"Attempt {index}: downloading {variable} to {destination}")
            client.retrieve(dataset, request, str(destination))
            print(f"Saved {destination}")
            return
        except Exception as exc:  # pragma: no cover - network/API dependent
            last_error = exc
            print(f"Attempt {index} failed for {variable}: {exc}")
    if last_error is None:
        raise RuntimeError(f"No request attempted for {variable}")
    raise last_error


def main() -> None:
    args = parse_args()
    variables = args.variables or list(DEFAULT_VARIABLES)
    years = build_years(args.start_year, args.end_year)
    months = build_months()

    output_dir = Path(args.output_dir).expanduser().resolve()
    output_dir.mkdir(parents=True, exist_ok=True)

    client = cdsapi.Client()

    for variable in variables:
        destination = target_path(output_dir, variable, args.start_year, args.end_year)
        if destination.exists() and not args.force:
            print(f"Skipping existing file: {destination}")
            continue
        retrieve_with_fallback(
            client=client,
            dataset=args.dataset,
            variable=variable,
            years=years,
            months=months,
            destination=destination,
        )


if __name__ == "__main__":
    main()
