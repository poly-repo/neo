import importlib.util
import tempfile
import unittest
from importlib.machinery import SourceFileLoader
from pathlib import Path
from unittest import mock


def load_module(name, path):
    loader = SourceFileLoader(name, str(path))
    spec = importlib.util.spec_from_loader(name, loader)
    module = importlib.util.module_from_spec(spec)
    loader.exec_module(module)
    return module


class TestNerdFontAssetAliases(unittest.TestCase):
    def test_neo_requirements_include_downloader_dependencies(self):
        requirements = set(
            Path('devex/editors/emacs/requirements.txt').read_text().splitlines()
        )

        self.assertTrue({'fonttools', 'requests'} <= requirements)

    def test_completion_marker_requires_an_installed_font(self):
        module = load_module(
            'fonts_role_completion_marker',
            Path('infra/ansible/roles/fonts/scripts/download'),
        )

        with tempfile.TemporaryDirectory() as base_dir:
            def install_font(_family, font_dir):
                (Path(font_dir) / 'Example-Regular.ttf').write_bytes(b'font')

            with mock.patch.object(
                module,
                'nerd_download_family',
                side_effect=install_font,
            ):
                module.main('Example:nerd', base_dir)

            marker = Path(base_dir, 'nerd', 'Example', '.installed')
            self.assertTrue(marker.exists())

        with tempfile.TemporaryDirectory() as base_dir:
            with mock.patch.object(
                module,
                'nerd_download_family',
                side_effect=RuntimeError('download failed'),
            ):
                with self.assertRaisesRegex(RuntimeError, 'download failed'):
                    module.main('Example:nerd', base_dir)

            marker = Path(base_dir, 'nerd', 'Example', '.installed')
            self.assertFalse(marker.exists())

    def test_ansible_downloader_uses_compressed_release_archives(self):
        module = load_module(
            'fonts_role_download',
            Path('infra/ansible/roles/fonts/scripts/download'),
        )

        self.assertEqual(module.nerd_release_asset_name('FontAwesome'), 'NerdFontsSymbolsOnly')
        self.assertEqual(module.nerd_release_archive_name('FontAwesome'), 'NerdFontsSymbolsOnly.tar.xz')
        self.assertEqual(module.nerd_release_archive_name('Iosevka'), 'Iosevka.tar.xz')

    def test_standalone_fetcher_uses_compressed_release_archives(self):
        module = load_module(
            'font_fetcher_download',
            Path('infra/tools/font_fetcher/o-font-fetcher'),
        )

        self.assertEqual(module.nerd_release_asset_name('FontAwesome'), 'NerdFontsSymbolsOnly')
        self.assertEqual(module.nerd_release_archive_name('FontAwesome'), 'NerdFontsSymbolsOnly.tar.xz')
        self.assertEqual(module.nerd_release_archive_name('Iosevka'), 'Iosevka.tar.xz')


if __name__ == '__main__':
    unittest.main()
