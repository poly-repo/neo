import importlib.util
import unittest
from importlib.machinery import SourceFileLoader
from pathlib import Path


def load_module(name, path):
    loader = SourceFileLoader(name, str(path))
    spec = importlib.util.spec_from_loader(name, loader)
    module = importlib.util.module_from_spec(spec)
    loader.exec_module(module)
    return module


class TestNerdFontAssetAliases(unittest.TestCase):
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
