/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;

/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;


CREATE DATABASE IF NOT EXISTS finansijsko;
USE finansijsko;
DROP TABLE IF EXISTS `banke`;
CREATE TABLE `banke` (
  `banka` int(3) unsigned NOT NULL DEFAULT '0',
  `naziv` varchar(45) NOT NULL DEFAULT '',
  PRIMARY KEY (`banka`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

DROP TABLE IF EXISTS `fin_2005`;
CREATE TABLE `fin_2005` (
  `godina` int(4) unsigned NOT NULL DEFAULT '0',
  `firma` int(9) unsigned NOT NULL DEFAULT '0',
  `datum` date NOT NULL DEFAULT '0000-00-00',
  `broj_naloga` int(9) unsigned NOT NULL DEFAULT '0',
  `opis` varchar(25) NOT NULL DEFAULT '',
  `konto` varchar(50) NOT NULL DEFAULT '',
  `duguje` decimal(15,2) NOT NULL DEFAULT '0.00',
  `potrazuje` decimal(15,2) NOT NULL DEFAULT '0.00',
  KEY `Index_1` (`godina`),
  KEY `Index_2` (`firma`),
  KEY `Index_3` (`broj_naloga`),
  KEY `Index_4` (`datum`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

DROP TABLE IF EXISTS `fin_2006`;
CREATE TABLE `fin_2006` (
  `godina` int(4) unsigned NOT NULL DEFAULT '0',
  `firma` int(9) unsigned NOT NULL DEFAULT '0',
  `datum` date NOT NULL DEFAULT '0000-00-00',
  `broj_naloga` int(9) unsigned NOT NULL DEFAULT '0',
  `opis` varchar(25) NOT NULL DEFAULT '',
  `konto` varchar(50) NOT NULL DEFAULT '',
  `duguje` decimal(15,2) NOT NULL DEFAULT '0.00',
  `potrazuje` decimal(15,2) NOT NULL DEFAULT '0.00',
  KEY `Index_1` (`godina`),
  KEY `Index_2` (`firma`),
  KEY `Index_3` (`broj_naloga`),
  KEY `Index_4` (`datum`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

DROP TABLE IF EXISTS `fin_rucno`;
CREATE TABLE `fin_rucno` (
  `godina` int(4) unsigned NOT NULL DEFAULT '0',
  `firma` int(9) unsigned NOT NULL DEFAULT '0',
  `datum` date NOT NULL DEFAULT '0000-00-00',
  `broj_naloga` int(9) unsigned NOT NULL DEFAULT '0',
  `opis` varchar(25) NOT NULL DEFAULT '',
  `konto` varchar(50) NOT NULL DEFAULT '',
  `duguje` decimal(15,2) NOT NULL DEFAULT '0.00',
  `potrazuje` decimal(15,2) NOT NULL DEFAULT '0.00',
  KEY `Index_1` (`godina`),
  KEY `Index_2` (`firma`),
  KEY `Index_3` (`broj_naloga`),
  KEY `Index_4` (`datum`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

DROP TABLE IF EXISTS `firme`;
CREATE TABLE `firme` (
  `godina` int(4) unsigned NOT NULL DEFAULT '0',
  `pib` int(9) unsigned NOT NULL DEFAULT '0',
  PRIMARY KEY (`godina`,`pib`),
  KEY `FK_firme_1` (`pib`),
  CONSTRAINT `FK_firme_1` FOREIGN KEY (`pib`) REFERENCES `partneri` (`pib`) ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='Firme za koje se vodi knjigovodstvo';

DROP TABLE IF EXISTS `interne`;
CREATE TABLE `interne` (
  `godina` int(4) unsigned NOT NULL DEFAULT '0',
  `firma` int(9) unsigned NOT NULL DEFAULT '0',
  `tip_interne` enum('I','U') NOT NULL DEFAULT 'I',
  `iz_magacina` int(2) unsigned NOT NULL DEFAULT '0',
  `tip_iz` int(1) unsigned NOT NULL DEFAULT '0',
  `u_magacin` int(2) unsigned NOT NULL DEFAULT '0',
  `tip_u` int(1) unsigned NOT NULL DEFAULT '0',
  `broj` int(5) unsigned NOT NULL DEFAULT '0',
  `datum` date NOT NULL DEFAULT '0000-00-00',
  `vpv1` decimal(12,2) NOT NULL DEFAULT '0.00',
  `vpv2` decimal(12,2) NOT NULL DEFAULT '0.00',
  `ruc` decimal(12,2) NOT NULL DEFAULT '0.00',
  `pdv_4` decimal(12,2) NOT NULL DEFAULT '0.00',
  `pdv_5` decimal(12,2) NOT NULL DEFAULT '0.00',
  `mpv` decimal(12,2) NOT NULL DEFAULT '0.00',
  `gpv` decimal(12,2) NOT NULL DEFAULT '0.00',
  `rmv` decimal(12,2) NOT NULL DEFAULT '0.00',
  `osnovica_pdv_4` decimal(12,2) NOT NULL DEFAULT '0.00',
  `osnovica_pdv_5` decimal(12,2) NOT NULL DEFAULT '0.00',
  `osnovica_pdv_0` decimal(12,2) NOT NULL DEFAULT '0.00',
  `uneo` varchar(20) NOT NULL DEFAULT '',
  `vreme` datetime NOT NULL DEFAULT '0000-00-00 00:00:00',
  PRIMARY KEY (`godina`,`firma`,`tip_interne`,`iz_magacina`,`tip_iz`,`u_magacin`,`tip_u`,`broj`),
  KEY `FK_interne_2` (`godina`,`firma`,`iz_magacina`,`tip_iz`),
  KEY `FK_interne_3` (`godina`,`firma`,`u_magacin`,`tip_u`),
  CONSTRAINT `FK_interne_1` FOREIGN KEY (`godina`, `firma`) REFERENCES `firme` (`godina`, `pib`) ON UPDATE CASCADE,
  CONSTRAINT `FK_interne_2` FOREIGN KEY (`godina`, `firma`, `iz_magacina`, `tip_iz`) REFERENCES `magacini` (`godina`, `firma`, `id`, `tip`) ON UPDATE CASCADE,
  CONSTRAINT `FK_interne_3` FOREIGN KEY (`godina`, `firma`, `u_magacin`, `tip_u`) REFERENCES `magacini` (`godina`, `firma`, `id`, `tip`) ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

DROP TABLE IF EXISTS `izdati_racuni`;
CREATE TABLE `izdati_racuni` (
  `godina` int(4) unsigned NOT NULL DEFAULT '0',
  `firma` int(9) unsigned NOT NULL DEFAULT '0',
  `broj` int(5) unsigned NOT NULL DEFAULT '0',
  `tip_kir` int(1) unsigned NOT NULL DEFAULT '0',
  `magacin` int(2) unsigned NOT NULL DEFAULT '0',
  `datum` date NOT NULL DEFAULT '0000-00-00',
  `kupac` int(9) unsigned NOT NULL DEFAULT '0',
  `iznos_racuna` decimal(12,2) NOT NULL DEFAULT '0.00',
  `osnovica_pdv_4` decimal(12,2) NOT NULL DEFAULT '0.00',
  `osnovica_pdv_5` decimal(12,2) NOT NULL DEFAULT '0.00',
  `osnovica_pdv_0` decimal(12,2) NOT NULL DEFAULT '0.00',
  `pdv_4` decimal(12,2) NOT NULL DEFAULT '0.00',
  `pdv_5` decimal(12,2) NOT NULL DEFAULT '0.00',
  `vpv` decimal(12,2) NOT NULL DEFAULT '0.00',
  `rabat` decimal(12,2) NOT NULL DEFAULT '0.00',
  `mpv` decimal(12,2) NOT NULL DEFAULT '0.00',
  `gpv` decimal(12,2) NOT NULL DEFAULT '0.00',
  `gotovina` decimal(12,2) NOT NULL DEFAULT '0.00',
  `cek` decimal(12,2) NOT NULL DEFAULT '0.00',
  `kartica` decimal(12,2) NOT NULL DEFAULT '0.00',
  `na_racun` decimal(12,2) NOT NULL DEFAULT '0.00',
  `usluge` decimal(12,2) NOT NULL DEFAULT '0.00',
  `kroz_fiskalnu` enum('NE','DA') NOT NULL DEFAULT 'NE',
  `uneo` varchar(20) NOT NULL DEFAULT '',
  `vreme` datetime NOT NULL DEFAULT '0000-00-00 00:00:00',
  PRIMARY KEY (`godina`,`firma`,`tip_kir`,`magacin`,`broj`),
  KEY `FK_izdati_racuni_2` (`kupac`),
  KEY `FK_izdati_racuni_3` (`godina`,`firma`,`magacin`,`tip_kir`),
  CONSTRAINT `FK_izdati_racuni_1` FOREIGN KEY (`godina`, `firma`) REFERENCES `firme` (`godina`, `pib`) ON UPDATE CASCADE,
  CONSTRAINT `FK_izdati_racuni_2` FOREIGN KEY (`kupac`) REFERENCES `partneri` (`pib`) ON UPDATE CASCADE,
  CONSTRAINT `FK_izdati_racuni_3` FOREIGN KEY (`godina`, `firma`, `magacin`, `tip_kir`) REFERENCES `magacini` (`godina`, `firma`, `id`, `tip`) ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

DROP TABLE IF EXISTS `izvodi`;
CREATE TABLE `izvodi` (
  `godina` int(4) unsigned NOT NULL DEFAULT '0',
  `firma` int(9) unsigned NOT NULL DEFAULT '0',
  `banka` int(3) unsigned NOT NULL DEFAULT '0',
  `broj` int(3) unsigned NOT NULL DEFAULT '0',
  `datum` date NOT NULL DEFAULT '0000-00-00',
  `duguje` decimal(12,2) NOT NULL DEFAULT '0.00',
  `potrazuje` decimal(12,2) NOT NULL DEFAULT '0.00',
  `uneo` varchar(20) NOT NULL DEFAULT '',
  `vreme` datetime NOT NULL DEFAULT '0000-00-00 00:00:00',
  PRIMARY KEY (`godina`,`firma`,`banka`,`broj`),
  KEY `FK_izvodi_2` (`banka`),
  CONSTRAINT `FK_izvodi_1` FOREIGN KEY (`godina`, `firma`) REFERENCES `firme` (`godina`, `pib`) ON UPDATE CASCADE,
  CONSTRAINT `FK_izvodi_2` FOREIGN KEY (`banka`) REFERENCES `banke` (`banka`) ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

DROP TABLE IF EXISTS `izvodi_stavke`;
CREATE TABLE `izvodi_stavke` (
  `godina` int(4) unsigned NOT NULL DEFAULT '0',
  `firma` int(9) unsigned NOT NULL DEFAULT '0',
  `banka` int(3) unsigned NOT NULL DEFAULT '0',
  `broj` int(3) unsigned NOT NULL DEFAULT '0',
  `tip` int(2) unsigned NOT NULL DEFAULT '0',
  `duguje` decimal(12,2) NOT NULL DEFAULT '0.00',
  `potrazuje` decimal(12,2) NOT NULL DEFAULT '0.00',
  `partner` int(9) unsigned NOT NULL DEFAULT '0',
  `magacin` int(2) unsigned NOT NULL DEFAULT '0',
  `konto` varchar(20) NOT NULL DEFAULT '',
  `uneo` varchar(20) NOT NULL DEFAULT '',
  `vreme` datetime NOT NULL DEFAULT '0000-00-00 00:00:00',
  KEY `FK_izvodi_stavke_1` (`godina`,`firma`,`banka`,`broj`),
  KEY `FK_izvodi_stavke_2` (`tip`),
  CONSTRAINT `FK_izvodi_stavke_1` FOREIGN KEY (`godina`, `firma`, `banka`, `broj`) REFERENCES `izvodi` (`godina`, `firma`, `banka`, `broj`) ON UPDATE CASCADE,
  CONSTRAINT `FK_izvodi_stavke_2` FOREIGN KEY (`tip`) REFERENCES `tip_promene_na_izvodu` (`tip`) ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

DROP TABLE IF EXISTS `kontni_okvir`;
CREATE TABLE `kontni_okvir` (
  `konto` varchar(5) NOT NULL DEFAULT '',
  `naziv` varchar(150) NOT NULL DEFAULT '',
  `proba` varchar(15) NOT NULL DEFAULT '',
  PRIMARY KEY (`konto`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

DROP TABLE IF EXISTS `magacini`;
CREATE TABLE `magacini` (
  `godina` int(4) unsigned NOT NULL DEFAULT '0',
  `firma` int(9) unsigned NOT NULL DEFAULT '0',
  `id` int(2) unsigned NOT NULL DEFAULT '0',
  `tip` int(1) unsigned NOT NULL DEFAULT '0',
  `naziv` varchar(40) NOT NULL DEFAULT '',
  PRIMARY KEY (`godina`,`firma`,`id`,`tip`),
  CONSTRAINT `FK_magacini_1` FOREIGN KEY (`godina`, `firma`) REFERENCES `firme` (`godina`, `pib`) ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

DROP TABLE IF EXISTS `mesta`;
CREATE TABLE `mesta` (
  `posta` int(10) unsigned NOT NULL DEFAULT '0',
  `naziv` varchar(30) NOT NULL DEFAULT '',
  PRIMARY KEY (`posta`),
  KEY `mesta` (`naziv`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='Mesta u SCG';

DROP TABLE IF EXISTS `nivelacije`;
CREATE TABLE `nivelacije` (
  `godina` int(4) unsigned NOT NULL DEFAULT '0',
  `firma` int(9) unsigned NOT NULL DEFAULT '0',
  `magacin` int(2) unsigned NOT NULL DEFAULT '0',
  `tip` int(1) unsigned NOT NULL DEFAULT '0',
  `broj` int(5) unsigned NOT NULL DEFAULT '0',
  `datum` date NOT NULL DEFAULT '0000-00-00',
  `ruc` decimal(13,2) NOT NULL DEFAULT '0.00',
  `osnovica_pdv_4` decimal(13,2) NOT NULL DEFAULT '0.00',
  `osnovica_pdv_5` decimal(13,2) NOT NULL DEFAULT '0.00',
  `osnovica_pdv_0` decimal(13,2) NOT NULL DEFAULT '0.00',
  `pdv_4` decimal(13,2) NOT NULL DEFAULT '0.00',
  `pdv_5` decimal(13,2) NOT NULL DEFAULT '0.00',
  `vrednost` decimal(13,2) NOT NULL DEFAULT '0.00',
  `uneo` varchar(20) NOT NULL DEFAULT '',
  `vreme` datetime NOT NULL DEFAULT '0000-00-00 00:00:00',
  PRIMARY KEY (`godina`,`firma`,`magacin`,`tip`,`broj`),
  CONSTRAINT `FK_nivelacije_1` FOREIGN KEY (`godina`, `firma`, `magacin`, `tip`) REFERENCES `magacini` (`godina`, `firma`, `id`, `tip`) ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

DROP TABLE IF EXISTS `otpisi`;
CREATE TABLE `otpisi` (
  `godina` int(4) unsigned NOT NULL DEFAULT '0',
  `firma` int(9) unsigned NOT NULL DEFAULT '0',
  `magacin` int(2) unsigned NOT NULL DEFAULT '0',
  `tip` int(1) unsigned NOT NULL DEFAULT '0',
  `broj` int(5) unsigned NOT NULL DEFAULT '0',
  `datum` date NOT NULL DEFAULT '0000-00-00',
  `osnovica_pdv_4` decimal(13,2) NOT NULL DEFAULT '0.00',
  `osnovica_pdv_5` decimal(13,2) NOT NULL DEFAULT '0.00',
  `osnovica_pdv_0` decimal(13,2) NOT NULL DEFAULT '0.00',
  `pdv_4` decimal(13,2) NOT NULL DEFAULT '0.00',
  `pdv_5` decimal(13,2) NOT NULL DEFAULT '0.00',
  `vrednost` decimal(13,2) NOT NULL DEFAULT '0.00',
  `konto` varchar(20) NOT NULL DEFAULT '',
  `uneo` varchar(20) NOT NULL DEFAULT '',
  `vreme` datetime NOT NULL DEFAULT '0000-00-00 00:00:00',
  PRIMARY KEY (`godina`,`firma`,`magacin`,`tip`,`broj`),
  CONSTRAINT `FK_otpisi_1` FOREIGN KEY (`godina`, `firma`, `magacin`, `tip`) REFERENCES `magacini` (`godina`, `firma`, `id`, `tip`) ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

DROP TABLE IF EXISTS `partneri`;
CREATE TABLE `partneri` (
  `pib` int(9) unsigned NOT NULL DEFAULT '0',
  `naziv` varchar(45) NOT NULL DEFAULT '',
  `prefiks` varchar(45) NOT NULL DEFAULT '',
  `sufiks` varchar(45) NOT NULL DEFAULT '',
  `vlasnik` varchar(45) NOT NULL DEFAULT '',
  `adresa` varchar(45) NOT NULL DEFAULT '',
  `mesto` int(5) unsigned NOT NULL DEFAULT '0',
  `broj_pdv_potvrde` decimal(9,0) unsigned NOT NULL DEFAULT '0',
  `pocetak_pdv` date NOT NULL DEFAULT '0000-00-00',
  `tekuci_1` varchar(20) NOT NULL DEFAULT '',
  `tekuci_2` varchar(20) NOT NULL DEFAULT '',
  `tekuci_3` varchar(20) NOT NULL DEFAULT '',
  `tekuci_4` varchar(20) NOT NULL DEFAULT '',
  `tekuci_5` varchar(20) NOT NULL DEFAULT '',
  `tekuci_6` varchar(20) NOT NULL DEFAULT '',
  `tekuci_7` varchar(20) NOT NULL DEFAULT '',
  `tekuci_8` varchar(20) NOT NULL DEFAULT '',
  `tekuci_9` varchar(20) NOT NULL DEFAULT '',
  `uneo` varchar(15) NOT NULL DEFAULT '',
  `vreme` datetime NOT NULL DEFAULT '0000-00-00 00:00:00',
  PRIMARY KEY (`pib`),
  KEY `Index_2` (`mesto`),
  CONSTRAINT `partneri_ibfk_1` FOREIGN KEY (`mesto`) REFERENCES `mesta` (`posta`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 ROW_FORMAT=DYNAMIC;

DROP TABLE IF EXISTS `pazari`;
CREATE TABLE `pazari` (
  `godina` int(4) unsigned NOT NULL DEFAULT '0',
  `firma` int(9) unsigned NOT NULL DEFAULT '0',
  `prodavnica` int(2) unsigned NOT NULL DEFAULT '0',
  `tip` int(1) unsigned NOT NULL DEFAULT '2',
  `datum` date NOT NULL DEFAULT '0000-00-00',
  `ukupan_pazar` decimal(12,2) NOT NULL DEFAULT '0.00',
  `bd` int(4) unsigned NOT NULL DEFAULT '0',
  `pa` decimal(12,2) NOT NULL DEFAULT '0.00',
  `pg` decimal(12,2) NOT NULL DEFAULT '0.00',
  `pdj` decimal(12,2) NOT NULL DEFAULT '0.00',
  `pe` decimal(12,2) NOT NULL DEFAULT '0.00',
  `pt` decimal(12,2) NOT NULL DEFAULT '0.00',
  `ea` decimal(12,2) NOT NULL DEFAULT '0.00',
  `eg` decimal(12,2) NOT NULL DEFAULT '0.00',
  `edj` decimal(12,2) NOT NULL DEFAULT '0.00',
  `ee` decimal(12,2) NOT NULL DEFAULT '0.00',
  `et` decimal(12,2) NOT NULL DEFAULT '0.00',
  `gotovina` decimal(12,2) NOT NULL DEFAULT '0.00',
  `cek` decimal(12,2) NOT NULL DEFAULT '0.00',
  `kartica` decimal(12,2) NOT NULL DEFAULT '0.00',
  `uneo` varchar(20) NOT NULL DEFAULT '',
  `vreme` datetime NOT NULL DEFAULT '0000-00-00 00:00:00',
  PRIMARY KEY (`godina`,`firma`,`prodavnica`,`datum`),
  KEY `FK_pazari_2` (`godina`,`firma`,`prodavnica`,`tip`),
  CONSTRAINT `FK_pazari_1` FOREIGN KEY (`godina`, `firma`) REFERENCES `firme` (`godina`, `pib`) ON UPDATE CASCADE,
  CONSTRAINT `FK_pazari_2` FOREIGN KEY (`godina`, `firma`, `prodavnica`, `tip`) REFERENCES `magacini` (`godina`, `firma`, `id`, `tip`) ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

DROP TABLE IF EXISTS `pocetno`;
CREATE TABLE `pocetno` (
  `godina` int(4) unsigned NOT NULL DEFAULT '0',
  `firma` int(9) unsigned NOT NULL DEFAULT '0',
  `partner` int(9) unsigned NOT NULL DEFAULT '0',
  `datum_knjizenja` date NOT NULL DEFAULT '0000-00-00',
  `datum_valute` date NOT NULL DEFAULT '0000-00-00',
  `opis` varchar(30) NOT NULL DEFAULT '',
  `kupac_dug` decimal(12,2) NOT NULL DEFAULT '0.00',
  `kupac_pot` decimal(12,2) NOT NULL DEFAULT '0.00',
  `dobavljac_dug` decimal(12,2) NOT NULL DEFAULT '0.00',
  `dobavljac_pot` decimal(12,2) NOT NULL DEFAULT '0.00',
  `uneo` varchar(20) NOT NULL DEFAULT '',
  `vreme` datetime NOT NULL DEFAULT '0000-00-00 00:00:00',
  KEY `FK_pocetno_2` (`partner`),
  KEY `FK_pocetno_1` (`godina`,`firma`),
  CONSTRAINT `FK_pocetno_1` FOREIGN KEY (`godina`, `firma`) REFERENCES `firme` (`godina`, `pib`) ON UPDATE CASCADE,
  CONSTRAINT `FK_pocetno_2` FOREIGN KEY (`partner`) REFERENCES `partneri` (`pib`) ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

DROP TABLE IF EXISTS `poljoprivrednici`;
CREATE TABLE `poljoprivrednici` (
  `jmbg` varchar(13) NOT NULL DEFAULT '',
  `prezime_i_ime` varchar(45) NOT NULL DEFAULT '',
  `adresa` varchar(45) NOT NULL DEFAULT '',
  `mesto` int(5) unsigned NOT NULL DEFAULT '0',
  `tekuci` varchar(20) NOT NULL DEFAULT '',
  `uneo` varchar(20) NOT NULL DEFAULT '',
  `vreme` datetime NOT NULL DEFAULT '0000-00-00 00:00:00',
  PRIMARY KEY (`jmbg`),
  KEY `FK_poljoprivrednici_1` (`mesto`),
  CONSTRAINT `FK_poljoprivrednici_1` FOREIGN KEY (`mesto`) REFERENCES `mesta` (`posta`) ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

DROP TABLE IF EXISTS `primljeni_racuni`;
CREATE TABLE `primljeni_racuni` (
  `godina` int(4) unsigned NOT NULL DEFAULT '0',
  `firma` int(9) unsigned NOT NULL DEFAULT '0',
  `broj` int(4) unsigned NOT NULL DEFAULT '0',
  `tip_kpr` int(1) unsigned NOT NULL DEFAULT '0',
  `datum_knjizenja` date NOT NULL DEFAULT '0000-00-00',
  `datum_izdavanja` date NOT NULL DEFAULT '0000-00-00',
  `datum_prijema_racuna` date NOT NULL DEFAULT '0000-00-00',
  `dobavljac` int(9) unsigned NOT NULL DEFAULT '0',
  `iznos_racuna` decimal(12,2) NOT NULL DEFAULT '0.00',
  `osnovica_pdv_4` decimal(12,2) NOT NULL DEFAULT '0.00',
  `osnovica_pdv_5` decimal(12,2) NOT NULL DEFAULT '0.00',
  `osnovica_pdv_0` decimal(12,2) NOT NULL DEFAULT '0.00',
  `pdv_4` decimal(12,2) NOT NULL DEFAULT '0.00',
  `pdv_5` decimal(12,2) NOT NULL DEFAULT '0.00',
  `pdv_4_bez` decimal(12,2) NOT NULL DEFAULT '0.00',
  `pdv_5_bez` decimal(12,2) NOT NULL DEFAULT '0.00',
  `poljoprivrednik` varchar(13) NOT NULL DEFAULT '0',
  `otkup` decimal(12,2) NOT NULL DEFAULT '0.00',
  `pdv_nadoknada` decimal(12,2) NOT NULL DEFAULT '0.00',
  `uneo` varchar(20) NOT NULL DEFAULT '',
  `vreme` datetime NOT NULL DEFAULT '0000-00-00 00:00:00',
  `tip_racuna` int(1) unsigned NOT NULL DEFAULT '0',
  `magacin` int(2) unsigned NOT NULL DEFAULT '0',
  `zavisni_troskovi` decimal(12,2) NOT NULL DEFAULT '0.00',
  `ruc` decimal(12,2) NOT NULL DEFAULT '0.00',
  `vpv` decimal(12,2) NOT NULL DEFAULT '0.00',
  `pdv_4_ukalkulisan` decimal(12,2) NOT NULL DEFAULT '0.00',
  `pdv_5_ukalkulisan` decimal(12,2) NOT NULL DEFAULT '0.00',
  `mpv` decimal(12,2) NOT NULL DEFAULT '0.00',
  `konto_troska` varchar(15) NOT NULL DEFAULT '',
  `otpremnica` varchar(20) NOT NULL DEFAULT '',
  `racun` varchar(20) NOT NULL DEFAULT '',
  `poziv_na_broj` varchar(20) NOT NULL DEFAULT '',
  `datum_valute` date NOT NULL DEFAULT '0000-00-00',
  PRIMARY KEY (`godina`,`firma`,`broj`),
  KEY `FK_primljeni_racuni_1` (`dobavljac`),
  KEY `FK_primljeni_racuni_2` (`poljoprivrednik`),
  CONSTRAINT `FK_primljeni_racuni_1` FOREIGN KEY (`godina`, `firma`) REFERENCES `firme` (`godina`, `pib`) ON UPDATE CASCADE,
  CONSTRAINT `FK_primljeni_racuni_2` FOREIGN KEY (`dobavljac`) REFERENCES `partneri` (`pib`) ON UPDATE CASCADE,
  CONSTRAINT `FK_primljeni_racuni_3` FOREIGN KEY (`poljoprivrednik`) REFERENCES `poljoprivrednici` (`jmbg`) ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

DROP TABLE IF EXISTS `radni_nalozi`;
CREATE TABLE `radni_nalozi` (
  `godina` int(4) unsigned NOT NULL DEFAULT '0',
  `firma` int(9) unsigned NOT NULL DEFAULT '0',
  `magacin_rm` int(2) unsigned NOT NULL DEFAULT '0',
  `tip_rm` int(1) unsigned NOT NULL DEFAULT '0',
  `magacin_gp` int(2) unsigned NOT NULL DEFAULT '0',
  `tip_gp` int(1) unsigned NOT NULL DEFAULT '0',
  `broj` int(5) unsigned NOT NULL DEFAULT '0',
  `datum` date NOT NULL DEFAULT '0000-00-00',
  `rmv` decimal(13,2) NOT NULL DEFAULT '0.00',
  `gpv` decimal(13,2) NOT NULL DEFAULT '0.00',
  `uneo` varchar(20) NOT NULL DEFAULT '',
  `vreme` datetime NOT NULL DEFAULT '0000-00-00 00:00:00',
  PRIMARY KEY (`godina`,`firma`,`magacin_rm`,`tip_rm`,`broj`),
  KEY `FK_radni_nalozi_2` (`godina`,`firma`,`magacin_gp`,`tip_gp`),
  CONSTRAINT `FK_radni_nalozi_1` FOREIGN KEY (`godina`, `firma`, `magacin_rm`, `tip_rm`) REFERENCES `magacini` (`godina`, `firma`, `id`, `tip`) ON UPDATE CASCADE,
  CONSTRAINT `FK_radni_nalozi_2` FOREIGN KEY (`godina`, `firma`, `magacin_gp`, `tip_gp`) REFERENCES `magacini` (`godina`, `firma`, `id`, `tip`) ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

DROP TABLE IF EXISTS `tip_promene_na_izvodu`;
CREATE TABLE `tip_promene_na_izvodu` (
  `tip` int(2) unsigned NOT NULL DEFAULT '0',
  `naziv` varchar(40) NOT NULL DEFAULT '',
  `po_partnerima` enum('NE','DA') NOT NULL DEFAULT 'NE',
  `po_tipu_magacina` int(1) unsigned NOT NULL DEFAULT '0',
  `samo_klasa` char(1) NOT NULL DEFAULT '',
  `samo_konto` varchar(15) NOT NULL DEFAULT '',
  `duguje` enum('NE','DA') NOT NULL DEFAULT 'NE',
  `potrazuje` enum('NE','DA') NOT NULL DEFAULT 'NE',
  PRIMARY KEY (`tip`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

CREATE DATABASE IF NOT EXISTS jagodinac;
USE jagodinac;

DROP TABLE IF EXISTS `artikli`;
CREATE TABLE `artikli` (
  `BARCODE` varchar(13) NOT NULL DEFAULT '',
  `NAZIV_DUG` varchar(60) NOT NULL DEFAULT '',
  `NAZIV_KRAT` varchar(18) NOT NULL DEFAULT '',
  `POREZ_GRP` smallint(1) NOT NULL DEFAULT '0',
  `JM` smallint(1) NOT NULL DEFAULT '0',
  `CENA_VP` double(9,2) NOT NULL DEFAULT '0.00',
  `CENA_MP` double(9,2) NOT NULL DEFAULT '0.00',
  `uneo` varchar(45) NOT NULL DEFAULT '',
  `vreme` datetime NOT NULL DEFAULT '0000-00-00 00:00:00',
  PRIMARY KEY (`BARCODE`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 ROW_FORMAT=DYNAMIC;

DROP TABLE IF EXISTS `otpremnice`;
CREATE TABLE `otpremnice` (
  `godina` int(4) unsigned NOT NULL DEFAULT '0',
  `firma` int(9) unsigned NOT NULL DEFAULT '0',
  `broj` smallint(5) unsigned NOT NULL DEFAULT '0',
  `kupac` int(9) unsigned NOT NULL DEFAULT '0',
  `datum` date NOT NULL DEFAULT '0000-00-00',
  `tip` int(1) unsigned NOT NULL DEFAULT '0',
  `magacin_id` int(2) unsigned NOT NULL DEFAULT '0',
  `isporuceno_u` varchar(40) NOT NULL DEFAULT '',
  `fakturisano` enum('NE','DA') NOT NULL DEFAULT 'NE',
  `uneo` varchar(20) NOT NULL DEFAULT '',
  `vreme` datetime NOT NULL DEFAULT '0000-00-00 00:00:00',
  PRIMARY KEY (`godina`,`firma`,`broj`,`tip`),
  KEY `Index_2` (`kupac`),
  KEY `Index_3` (`godina`,`firma`,`magacin_id`,`tip`),
  CONSTRAINT `FK_otpremnice_1` FOREIGN KEY (`kupac`) REFERENCES `finansijsko`.`partneri` (`pib`) ON UPDATE CASCADE,
  CONSTRAINT `FK_otpremnice_2` FOREIGN KEY (`godina`, `firma`, `magacin_id`, `tip`) REFERENCES `finansijsko`.`magacini` (`godina`, `firma`, `id`, `tip`) ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

DROP TABLE IF EXISTS `pocetno`;
CREATE TABLE `pocetno` (
  `datum` date DEFAULT NULL,
  `kupac` int(9) unsigned DEFAULT NULL,
  `duguje` decimal(13,2) DEFAULT NULL,
  `potrazuje` decimal(13,2) DEFAULT NULL,
  KEY `FK_pocetno_1` (`kupac`),
  CONSTRAINT `FK_pocetno_1` FOREIGN KEY (`kupac`) REFERENCES `finansijsko`.`partneri` (`pib`) ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

DROP TABLE IF EXISTS `racuni`;
CREATE TABLE `racuni` (
  `godina` int(4) unsigned NOT NULL DEFAULT '0',
  `firma` int(9) unsigned NOT NULL DEFAULT '0',
  `broj` smallint(5) unsigned NOT NULL DEFAULT '0',
  `kupac` int(9) unsigned NOT NULL DEFAULT '0',
  `datum` date NOT NULL DEFAULT '0000-00-00',
  `tip` int(1) unsigned NOT NULL DEFAULT '0',
  `valuta` date NOT NULL DEFAULT '0000-00-00',
  `magacin_id` int(2) unsigned NOT NULL DEFAULT '0',
  `isporuceno_u` varchar(40) NOT NULL DEFAULT '',
  `osnovica_pdv_4` decimal(13,2) NOT NULL DEFAULT '0.00',
  `osnovica_pdv_5` decimal(13,2) NOT NULL DEFAULT '0.00',
  `osnovica_pdv_0` decimal(13,2) NOT NULL DEFAULT '0.00',
  `pdv_4` decimal(13,2) NOT NULL DEFAULT '0.00',
  `pdv_5` decimal(13,2) NOT NULL DEFAULT '0.00',
  `za_uplatu` decimal(13,2) NOT NULL DEFAULT '0.00',
  `otpremnice` text,
  `uneo` varchar(20) NOT NULL DEFAULT '',
  `vreme` datetime NOT NULL DEFAULT '0000-00-00 00:00:00',
  `avansni` enum('NE','DA') NOT NULL DEFAULT 'NE',
  `vrednost_pdv_4` decimal(13,2) NOT NULL DEFAULT '0.00',
  `vrednost_pdv_5` decimal(13,2) NOT NULL DEFAULT '0.00',
  `vrednost_pdv_0` decimal(13,2) NOT NULL DEFAULT '0.00',
  `rabat_pdv_4` decimal(13,2) NOT NULL DEFAULT '0.00',
  `rabat_pdv_5` decimal(13,2) NOT NULL DEFAULT '0.00',
  `rabat_pdv_0` decimal(13,2) NOT NULL DEFAULT '0.00',
  `avans_osnovica_pdv_4` decimal(13,2) NOT NULL DEFAULT '0.00',
  `avans_osnovica_pdv_5` decimal(13,2) NOT NULL DEFAULT '0.00',
  `avans_osnovica_pdv_0` decimal(13,2) NOT NULL DEFAULT '0.00',
  `avans_pdv_4` decimal(13,2) NOT NULL DEFAULT '0.00',
  `avans_pdv_5` decimal(13,2) NOT NULL DEFAULT '0.00',
  `fiskalna_kasa` varchar(8) NOT NULL DEFAULT '',
  `broj_isecka` int(6) unsigned NOT NULL DEFAULT '0',
  PRIMARY KEY (`godina`,`firma`,`broj`,`tip`),
  KEY `Index_2` (`magacin_id`),
  KEY `Index_1` (`kupac`),
  KEY `Index_4` (`godina`,`firma`,`magacin_id`,`tip`),
  CONSTRAINT `FK_racuni_1` FOREIGN KEY (`kupac`) REFERENCES `finansijsko`.`partneri` (`pib`) ON UPDATE CASCADE,
  CONSTRAINT `FK_racuni_2` FOREIGN KEY (`godina`, `firma`, `magacin_id`, `tip`) REFERENCES `finansijsko`.`magacini` (`godina`, `firma`, `id`, `tip`) ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

DROP TABLE IF EXISTS `stavke_na_internim`;
CREATE TABLE `stavke_na_internim` (
  `idstavke_na_internim` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `godina` int(4) unsigned NOT NULL DEFAULT '0',
  `firma` int(9) unsigned NOT NULL DEFAULT '0',
  `tip_interne` enum('I','U') NOT NULL DEFAULT 'I',
  `iz_magacina` int(2) unsigned NOT NULL DEFAULT '0',
  `tip_iz` int(1) unsigned NOT NULL DEFAULT '0',
  `u_magacin` int(2) unsigned NOT NULL DEFAULT '0',
  `tip_u` int(1) unsigned NOT NULL DEFAULT '0',
  `broj` int(5) unsigned NOT NULL DEFAULT '0',
  `barcode` varchar(13) NOT NULL DEFAULT '',
  `kolicina` decimal(9,3) NOT NULL DEFAULT '0.000',
  `vp_cena` decimal(9,2) NOT NULL DEFAULT '0.00',
  `mp_cena` decimal(9,2) NOT NULL DEFAULT '0.00',
  `rm_cena` decimal(9,2) NOT NULL DEFAULT '0.00',
  `gp_cena` decimal(9,2) NOT NULL DEFAULT '0.00',
  `uneo` varchar(20) NOT NULL DEFAULT '',
  `vreme` datetime NOT NULL DEFAULT '0000-00-00 00:00:00',
  PRIMARY KEY (`idstavke_na_internim`),
  KEY `Index_2` (`godina`,`firma`,`tip_interne`,`iz_magacina`,`tip_iz`,`u_magacin`,`tip_u`,`broj`),
  KEY `Index_3` (`barcode`),
  CONSTRAINT `FK_stavke_na_internim_1` FOREIGN KEY (`godina`, `firma`, `tip_interne`, `iz_magacina`, `tip_iz`, `u_magacin`, `tip_u`, `broj`) REFERENCES `finansijsko`.`interne` (`godina`, `firma`, `tip_interne`, `iz_magacina`, `tip_iz`, `u_magacin`, `tip_u`, `broj`) ON UPDATE CASCADE,
  CONSTRAINT `FK_stavke_na_internim_2` FOREIGN KEY (`barcode`) REFERENCES `artikli` (`BARCODE`) ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

DROP TABLE IF EXISTS `stavke_na_otpremnicama`;
CREATE TABLE `stavke_na_otpremnicama` (
  `godina` int(4) unsigned NOT NULL DEFAULT '0',
  `firma` int(9) unsigned NOT NULL DEFAULT '0',
  `otpremnica` smallint(5) unsigned NOT NULL DEFAULT '0',
  `tip` int(1) unsigned NOT NULL DEFAULT '0',
  `barcode` varchar(13) NOT NULL DEFAULT '',
  `kolicina` decimal(9,3) NOT NULL DEFAULT '0.000',
  `uneo` varchar(20) NOT NULL DEFAULT '',
  `vreme` datetime NOT NULL DEFAULT '0000-00-00 00:00:00',
  KEY `Index_3` (`godina`,`firma`,`otpremnica`,`tip`),
  KEY `Index_2` (`barcode`),
  CONSTRAINT `FK_stavke_na_otpremnicama_1` FOREIGN KEY (`godina`, `firma`, `otpremnica`, `tip`) REFERENCES `otpremnice` (`godina`, `firma`, `broj`, `tip`) ON UPDATE CASCADE,
  CONSTRAINT `FK_stavke_na_otpremnicama_2` FOREIGN KEY (`barcode`) REFERENCES `artikli` (`BARCODE`) ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

DROP TABLE IF EXISTS `stavke_na_racunima`;
CREATE TABLE `stavke_na_racunima` (
  `godina` int(4) unsigned NOT NULL DEFAULT '0',
  `firma` int(9) unsigned NOT NULL DEFAULT '0',
  `racun` smallint(5) unsigned NOT NULL DEFAULT '0',
  `tip` int(1) unsigned NOT NULL DEFAULT '0',
  `magacin_id` int(2) unsigned NOT NULL DEFAULT '0',
  `barcode` varchar(13) NOT NULL DEFAULT '',
  `kolicina` decimal(9,3) NOT NULL DEFAULT '0.000',
  `cena` decimal(9,2) NOT NULL DEFAULT '0.00',
  `vrednost` decimal(9,2) NOT NULL DEFAULT '0.00',
  `popust_procenat` decimal(5,2) NOT NULL DEFAULT '0.00',
  `popust_iznos` decimal(9,2) NOT NULL DEFAULT '0.00',
  `osnovica` decimal(9,2) NOT NULL DEFAULT '0.00',
  `pdv` decimal(9,2) NOT NULL DEFAULT '0.00',
  `uneo` varchar(20) NOT NULL DEFAULT '',
  `vreme` datetime NOT NULL DEFAULT '0000-00-00 00:00:00',
  `avansni` enum('NE','DA') NOT NULL DEFAULT 'NE',
  KEY `Index_2` (`barcode`),
  KEY `Index_1` (`godina`,`firma`,`racun`,`tip`),
  KEY `FK_stavke_na_racunima_3` (`godina`,`firma`,`magacin_id`,`tip`),
  CONSTRAINT `FK_stavke_na_racunima_1` FOREIGN KEY (`godina`, `firma`, `racun`, `tip`) REFERENCES `racuni` (`godina`, `firma`, `broj`, `tip`) ON UPDATE CASCADE,
  CONSTRAINT `FK_stavke_na_racunima_2` FOREIGN KEY (`barcode`) REFERENCES `artikli` (`BARCODE`) ON UPDATE CASCADE,
  CONSTRAINT `FK_stavke_na_racunima_3` FOREIGN KEY (`godina`, `firma`, `magacin_id`, `tip`) REFERENCES `finansijsko`.`magacini` (`godina`, `firma`, `id`, `tip`) ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='InnoDB free: 19456 kB; (`barcode`) REFER `jagodinac/artikli`';

DROP TABLE IF EXISTS `stavke_na_ulazu`;
CREATE TABLE `stavke_na_ulazu` (
  `godina` int(4) unsigned NOT NULL DEFAULT '0',
  `firma` int(9) unsigned NOT NULL DEFAULT '0',
  `broj` int(4) unsigned NOT NULL DEFAULT '0',
  `tip` int(1) unsigned NOT NULL DEFAULT '0',
  `magacin_id` int(2) unsigned NOT NULL DEFAULT '0',
  `barcode` varchar(13) NOT NULL DEFAULT '',
  `kolicina` decimal(9,3) NOT NULL DEFAULT '0.000',
  `vrednost` decimal(9,2) NOT NULL DEFAULT '0.00',
  `prodajna_cena` decimal(9,2) NOT NULL DEFAULT '0.00',
  `uneo` varchar(20) NOT NULL DEFAULT '',
  `vreme` datetime NOT NULL DEFAULT '0000-00-00 00:00:00',
  id int,
  KEY `Index_1` (`godina`,`firma`,`broj`),
  KEY `Index_2` (`godina`,`firma`,`magacin_id`,`tip`),
  KEY `Index_3` (`barcode`),
  CONSTRAINT `FK_stavke_na_ulazu_1` FOREIGN KEY (`godina`, `firma`, `broj`) REFERENCES `finansijsko`.`primljeni_racuni` (`godina`, `firma`, `broj`) ON UPDATE CASCADE,
  CONSTRAINT `FK_stavke_na_ulazu_2` FOREIGN KEY (`godina`, `firma`, `magacin_id`, `tip`) REFERENCES `finansijsko`.`magacini` (`godina`, `firma`, `id`, `tip`) ON UPDATE CASCADE,
  CONSTRAINT `FK_stavke_na_ulazu_3` FOREIGN KEY (`barcode`) REFERENCES `artikli` (`BARCODE`) ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

DROP TABLE IF EXISTS `test1`;
CREATE TABLE `test1` (
  `godina` int(4) unsigned NOT NULL DEFAULT '0',
  `firma` int(9) unsigned NOT NULL DEFAULT '0',
  `broj` int(4) unsigned NOT NULL DEFAULT '0',
  `tip` int(1) unsigned NOT NULL DEFAULT '0',
  `magacin_id` int(2) unsigned NOT NULL DEFAULT '0',
  `barcode` varchar(13) NOT NULL DEFAULT '',
  `kolicina` decimal(9,3) NOT NULL DEFAULT '0.000',
  `vrednost` decimal(9,2) NOT NULL DEFAULT '0.00',
  `prodajna_cena` decimal(9,2) NOT NULL DEFAULT '0.00',
  `uneo` varchar(20) NOT NULL DEFAULT '',
  `vreme` datetime NOT NULL DEFAULT '0000-00-00 00:00:00',
  id int,
  KEY `Index_1` (`godina`,`firma`,`broj`),
  KEY `Index_2` (`godina`,`firma`,`magacin_id`,`tip`),
  KEY `Index_3` (`barcode`),
  CONSTRAINT `FK_test1_1` FOREIGN KEY (`godina`, `firma`, `broj`) REFERENCES `finansijsko`.`primljeni_racuni` (`godina`, `firma`, `broj`) ON DELETE RESTRICT ON UPDATE CASCADE,
  CONSTRAINT `FK_test1_2` FOREIGN KEY (`godina`, `firma`, `magacin_id`, `tip`) REFERENCES `finansijsko`.`magacini` (`godina`, `firma`, `id`, `tip`) ON DELETE NO ACTION,
  CONSTRAINT `FK_test1_3` FOREIGN KEY (`id`) REFERENCES `finansijsko`.`stavke_na_ulazu` (`id`) ON UPDATE CASCADE ON DELETE SET NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;

